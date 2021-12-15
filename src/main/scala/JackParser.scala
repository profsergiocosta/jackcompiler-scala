
package jackcompiler


    
import jackcompiler.ast as ast
import jackcompiler.TSymbol

    
class JackParser (val fName:String) {

    val jt = new JackTokenizer (fName)

    var currToken : Token = null
    var peekToken : Token = jt.nextToken()


    def nextToken ()  = {
        currToken = peekToken
        if (jt.hasMoreTokens())
            peekToken = jt.nextToken()
    }

    def parseStatements () : List[ast.Statement] = {
        if (!jt.hasMoreTokens ()) 
            return Nil
        else
            return parseStatement() :: parseStatements()
    }


    def parseStatement () : ast.Statement = {
        return parseLetStatement()
    }

    def parseLetStatement () : ast.LetStatement = {
        expectPeek(TKeyword ("let"))
        
        val id = parseIdentifier();
        
        expectPeek(TSymbol('='));
        
        val exp = parseExpression()
        val st = ast.LetStatement(id, exp)
        currToken
        expectPeek(TSymbol(';'));
        
        return st;
    }



    def parseIdentifier () : ast.Identifier = {
        
        expectPeek(TIdentifier(null))
        
        currToken match {
            case TIdentifier (varname) => {
                
                peekToken match {
                    case TSymbol ('[') =>  {
                        nextToken()
                        val exp = parseExpression()
                        expectPeek(TSymbol(']'))
                        return ast.IndexVariable(varname,exp)

                    }
                    case _ => return ast.Variable(varname)
                }
                
            }
            case _ => throw new Exception ("identifier expected")
        }
    }

 

    def isOperator (token :Token) : Boolean = {
        token match {
            case TSymbol ('+')| TSymbol ('-')
                 |TSymbol ('*') | TSymbol ('/') 
                 |TSymbol ('&') | TSymbol ('|')  
                 |TSymbol ('>') | TSymbol ('<') | TSymbol ('=')
                    => return true
            case _ => false
        }
    }

    def parseExpression()  : ast.Expression  = {
        var exp = parseTerm()
        while (isOperator (peekToken) ) {
            nextToken()
            val op = currToken match { case TSymbol (op) => op} // ??
            exp = ast.BinaryExpression (exp, op, parseTerm())
        }
        return exp
    }

        

    def parseTerm () : ast.Expression = {
        peekToken match {
            case TIntConst (value) => {
                nextToken()
                return ast.IntegerLiteral(value)
            }

            case TSymbol('(') => {
                expectPeek(TSymbol('(')) // avançar
                var exp = parseExpression()
                expectPeek(TSymbol(')')) // avançar 
                return exp
            }
        }
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
