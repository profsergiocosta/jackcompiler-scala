
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
        if (!jt.hasMoreTokens () || !isStatement (peekToken) ) 
            return Nil
        else
            return parseStatement() :: parseStatements()
    }


    def parseStatement () : ast.Statement = {
    
        peekToken match {
            case TKeyword("let") => {
                return    parseLetStatement()
            }
            case TKeyword("if") => {
                return    parseIfStatement()
            }

            case TKeyword("while") => {
                return    parseWhileStatement()
            }


        }

       
    }

    def parseLetStatement () : ast.LetStatement = {
        expectPeek(TKeyword ("let"))
        
        val id = parseIdentifier();
        
        expectPeek(TSymbol('='));
        
        val exp = parseExpression()
        val st = ast.LetStatement(id, exp)

        expectPeek(TSymbol(';'));
        
        return st;
    }

    def parseIfStatement () : ast.IfStatement = {
        expectPeek(TKeyword ("if"))
        expectPeek(TSymbol ('('))
        var cond = parseExpression()
        expectPeek(TSymbol (')'))
        expectPeek(TSymbol ('{'))
        var thenSts = parseStatements()
        expectPeek(TSymbol ('}'))
        peekToken match {    
            case TKeyword("else") => {
                expectPeek(TKeyword("else"))
                expectPeek(TSymbol ('{'))
                var elseSts = parseStatements()
                expectPeek(TSymbol ('}'))
                return ast.IfStatement(cond, ast.Statements(thenSts), Some(ast.Statements(elseSts)))        
            }

            case _ => return ast.IfStatement(cond, ast.Statements(thenSts))        

        }
    }

    def parseWhileStatement () : ast.WhileStatement = {
        expectPeek(TKeyword ("while"))
        expectPeek(TSymbol ('('))
        var cond = parseExpression()
        expectPeek(TSymbol (')'))
        expectPeek(TSymbol ('{'))
        var body = parseStatements()
        expectPeek(TSymbol ('}'))
        return ast.WhileStatement(cond, ast.Statements(body))        

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

    def isStatement (token :Token) : Boolean = {
        token match {
            case TKeyword ("if")| TKeyword ("let")
                 |TKeyword ("while") | TKeyword ("do") |TKeyword ("return")
                    => return true
            case _ => false
        }
    }
  
  
}
