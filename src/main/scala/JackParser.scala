
package jackcompiler


    
import jackcompiler.ast as ast

    
class JackParser (val fName:String) {

    val jt = new JackTokenizer (fName)

    var currToken : Token = null
    var peekToken : Token = jt.nextToken()


    def nextToken ()  = {
        currToken = peekToken
        peekToken = jt.nextToken()
    }

    def parseLetStatement () = {
        expectPeek(TKeyword ("let"))
        expectPeek(TIdentifier (null))
        val id = tokenToAST(currToken)
        println(id)
    }

    def tokenToAST (tk:Token) : ast.Node = {
            var node = currToken match  {
                case TIdentifier (varname) => ast.Identifier (varname)
            }
            return node
    }

   
    def expectPeek (tk:Token)  = {
        peekToken match {
            case TIdentifier (_) => {
                tk match {
                    case TIdentifier (_) =>  nextToken()
                    case _ => throw new Exception ("erro")
                }
            }
            case _ => {
                if (peekToken == tk) {
                        nextToken()
                    }else {
                        throw new Exception (tk.toString() + peekToken.toString())
                    }
                }
            }
        }  
  
}
