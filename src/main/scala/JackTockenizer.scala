
package JackCompiler

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Stack

abstract class Token
case class TKeyword(s:String) extends Token
case class TSymbol (c:Char) extends Token
case class TIdentifier(s:String) extends Token
case class TStringConst(s:String) extends Token
case class TIntConst(i:Int) extends Token
case class TUnknow(s:String) extends Token

class JackTokenizer (val fName:String) {
    //constructor
    val keywords = Set("int","class","constructor","function","method","field","static","var","char","boolean","void","true","false","null","this","let","do","if","else","while","return");
    val source = scala.io.Source.fromFile(fName)
    val s = (source.getLines mkString "\n").replaceAll("""(//.*\n)|(/\*(.|\n)*?\*/)"""," ") // remove comentarios
    //val pattern = """("\w*")|[a-zA-Z_]+[a-zA-Z0-9_]*|[0-9]+|[+|*|/|\-|{|}|(|)|\[|\]|\.|,|;|<|>|=|~]""".r
    val pattern = """(".*")|[a-zA-Z_]+[a-zA-Z0-9_]*|[0-9]+|[+|*|/|\-|{|}|(|)|\[|\]|\.|,|;|<|>|=|~]""".r
    val tokens = pattern findAllIn s
    var currToken = ""

    
    // methods
    // private
    private def isSymbol(s:String) =  "{}()[].,;+-*/&|<>=~".indexOf(s) != -1
    private def isStringConst (s:String) =  (s.charAt (0) == '\"') && (s.charAt (s.length-1) == '\"');
    private def isIntConst(s:String)=  s.matches("[0-9]+")

    def advance ()  = currToken = tokens.next
    def updateCurrToken (tk:String)  = currToken = tk
    
    def hasMoreTokens () = tokens.hasNext
    def getToken() :Token = {
       if (currToken == "") throw new Exception ("token not initialized, execute advance method first")

       if (keywords contains currToken)     TKeyword(currToken)
       else if (isSymbol(currToken))         TSymbol(currToken.charAt(0))
       else if (isStringConst(currToken))    TStringConst(currToken.slice (1,currToken.length-1))
       else if (isIntConst(currToken))       TIntConst(currToken.toInt)
       else                                  TIdentifier(currToken)
    }

    def getTokenAsString() = currToken

    def  tagToken () : String = tagTokenBy(getToken()) 

    def  tagTokenBy (t:Token) : String = {
            var s = t match {
            case TKeyword(k) =>  ("<keyword>"+k+"</keyword>")
            case TIdentifier(i) =>  ("<identifier>"+i+"</identifier>")
            case TSymbol(s) =>  "<symbol>".concat (
                                            s  match {
                                                    case '<' => "&lt;"
                                                    case '>' => "&gt;"
                                                    case '"' => "&quot;"
                                                    case '&' => "&amp;"
                                                    case _   => ""+s
                                            }).concat ( "</symbol>")
            case TIntConst(i) =>  ("<intConst>"+i+"</intConst>")
            case TStringConst(s) =>  ("<stringConst>"+s+"</stringConst>")
          }
          s = s +"\n"
          return s
    }
}
