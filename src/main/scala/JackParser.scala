
package jackcompiler


    
import jackcompiler.ast as ast
import jackcompiler.TSymbol
import jackcompiler.ast.ReturnStatement
import java.security.Identity
import scala.annotation.varargs

    
class JackParser (val fName:String) {

    val jt = new JackTokenizer (fName)

    var currToken : Token = null
    var peekToken : Token = jt.nextToken()


    def nextToken ()  = {
        currToken = peekToken
        if (jt.hasMoreTokens())
            peekToken = jt.nextToken()
    }

    def parseClass () = {
        expectPeek(TKeyword("class"));
        expectPeek(TIdentifier(null)); // nao importa ainda o nome do identificador
        expectPeek(TSymbol('{'));
        parseClassVarDec()
        parseSubroutineDec()
        expectPeek(TSymbol('}'));
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

            case TKeyword("return") => {
               return    parseReturnStatement()
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


    def parseVarDec () : List[ast.VarDeclaration] = {
        peekToken match {
            case TKeyword ("var") => {
                    nextToken()
                    var t = parseType()
                    var vardec = parseListVarDeclaration("var",t) ++ parseVarDec()
                    return vardec

                } 
                case _ => Nil
            }
    }
    

    def parseClassVarDec () : List[ast.VarDeclaration] = {
        peekToken match {
            case TKeyword (k)  => k match {
                case "field" | "static" => {
                    nextToken()
                    var t = parseType()
                    var vardec = parseListVarDeclaration(k,t) ++ parseClassVarDec()
                    return vardec

                } 
                case _ => Nil
            }
            case _ => Nil
        }
    }


    def parseListVarDeclaration(kind: String, varType: String) : List[ast.VarDeclaration] = {
      
      peekToken match {
        case TIdentifier (name) => {
              nextToken()
              var vardec = ast.VarDeclaration(kind, varType , name)
              if (peekToken == TSymbol (',') ) {
                  nextToken()
                  return vardec :: parseListVarDeclaration(kind, varType)
              } else {
                  expectPeek(TSymbol(';'))
                  return vardec :: Nil
              }
          } 
                  
        case _ =>  throw Exception ("erro: identifier expected")
      }
      
    }

    def parseSubroutineBody () = {
        expectPeek(TSymbol('{'))
        parseVarDec()
        parseStatements()
        expectPeek(TSymbol('}'))
    }

    def parseSubroutineDec()  = {
        peekToken match {
            case TKeyword (k) => k match {

                case "constructor"| "function"| "method" => {
                    nextToken()
                    parseType()
                    expectPeek(TIdentifier(null))
                    var varname = currToken match { case TIdentifier (v) => v}
                    expectPeek(TSymbol('('))
                    expectPeek(TSymbol(')'))
                    parseSubroutineBody()
                }

                case _ => {}

            }
            case _ => {}
        }
    }

    def parseType () : String = {
        peekToken match {
            case TKeyword (k) => k match {
                case "int"|"char"|"boolean"|"void" =>  {
                    nextToken()
                    return k
                }
                case _ =>  throw Exception ("erro: type expected")
            }
            case TIdentifier (varType) =>  {
                nextToken()
                return varType
            }
            case _ => throw Exception ("erro: type expected")
      }
    }

    def parseReturnStatement () : ast.ReturnStatement = {
        expectPeek(TKeyword ("return"))
       
        var stmt = peekToken match {
            case TSymbol (';') => ReturnStatement ()
            case _ => ReturnStatement (Some (parseExpression()))
        }
        expectPeek(TSymbol (';'))
        return stmt
      
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
