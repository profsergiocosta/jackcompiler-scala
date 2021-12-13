
package jackcompiler


    
import jackcompiler.ast as ast

    
class JackParser (val fName:String) {

    val jt = new JackTokenizer (fName)

    var currToken : Token = null
    var peekToken : Token = jt.nextToken()


    def nextToken ()  = {
        currToken = peekToken
        if (jt.hasMoreTokens())
            peekToken = jt.nextToken()
    }

    def parseLetStatement () : ast.Node = {
        expectPeek(TKeyword ("let"))
        val id = parseIdentifier();
        expectPeek(TSymbol('='));
        val exp = parseExpression()
        val st = ast.LetStatement(id, exp)
        println(currToken)
        println(peekToken)
        expectPeek(TSymbol(';'));
        return st;
    }

    def parseIdentifier () : ast.Identifier = {
        peekToken match {
            case TIdentifier (varname) => {
                nextToken()
                return ast.Identifier(varname)
            }
            case _ => throw new Exception ("identifier expected")
        }
    }

    def parseExpression()  : ast.Expression  = {
        return parseTerm()
    }

    def parseTerm () : ast.Expression = {
        peekToken match {
            case TIntConst (value) => {
                nextToken()
                return ast.IntegerLiteral(value)
            }
        }
    }

    private def toast (tk:Token) : ast.Node = {
            var node = currToken match  {
                case TIdentifier (varname) => ast.Identifier (varname)
            }
            return node
    }

   
    private def expectPeek (tk:Token)  = {
        tk match {
            case TIdentifier (_) => {
                peekToken match {
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
