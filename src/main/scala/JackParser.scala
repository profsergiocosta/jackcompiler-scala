
package jackcompiler


    
import jackcompiler.ast as ast
import jackcompiler.TSymbol
import jackcompiler.ast.ReturnStatement
import java.security.Identity
import scala.annotation.varargs

    
class JackParser (val source:String) {

    val jt = new JackTokenizer (source)

    var currToken : Token = null
    var peekToken : Token = jt.nextToken()


    def nextToken ()  = {
        currToken = peekToken
        if (jt.hasMoreTokens())
            peekToken = jt.nextToken()
    }

    def parseClass () : ast.ClassDec = {
        expectPeek(TKeyword("class"));
        expectPeek(TIdentifier(null)); // nao importa ainda o nome do identificador
        var classname = currToken match { case TIdentifier (v) => v}
        expectPeek(TSymbol('{'));
        var vardecs = parseClassVarDec()
        var subs = parseSubroutineDec()
        expectPeek(TSymbol('}'));
        return ast.ClassDec (classname, subs)
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


            case TKeyword("do") => {
               return    parseDoStatement()
            }



        }

       
    }

    def parseDoStatement () : ast.DoStatement = {
        expectPeek(TKeyword ("do"))
        expectPeek(TIdentifier (null))
        var call = parseSubroutineCall()
        expectPeek(TSymbol(';'));
        return ast.DoStatement(call)
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


    def parseVarDec () : Unit = {
        peekToken match {
            case TKeyword ("var") => {
                    nextToken()
                    var t = parseType()
                    parseListVarDeclaration("var",t) 
                    parseVarDec()

                } 
                case _ => return
            }
    }
    

    def parseClassVarDec () : Unit = {
        peekToken match {
            case TKeyword (k)  => k match {
                case "field" | "static" => {
                    nextToken()
                    var t = parseType()
                    parseListVarDeclaration(k,t)
                    parseClassVarDec()
                } 
                case _ => return
            }
            case _ => return
        }
    }

    def parseParameterList() : Unit = {
        if (peekTokenIs(TSymbol(')'))) return

        var t = parseType()
        expectPeek(TIdentifier(null))
        if (peekTokenIs(TSymbol(','))) {
            expectPeek(TSymbol(','))
            parseParameterList()
        }
    }


    def parseListVarDeclaration(kind: String, varType: String) : Unit = {
      
      peekToken match {
        case TIdentifier (name) => {
              nextToken()
              //var vardec = ast.VarDeclaration(kind, varType , name)
              if (peekToken == TSymbol (',') ) {
                  nextToken()
                  parseListVarDeclaration(kind, varType)
              } else {
                  expectPeek(TSymbol(';'))
              }
          } 
                  
        case _ =>  throw Exception ("erro: identifier expected")
      }
      
    }

    def parseSubroutineBody () : ast.SubroutineBody = {
        expectPeek(TSymbol('{'))
        var vardecs = parseVarDec()
        var sts = parseStatements()
        expectPeek(TSymbol('}'))
        return ast.SubroutineBody( ast.Statements(sts))
    }

    def parseSubroutineDec() : List[ast.Subroutine]  = {
        peekToken match {
            case TKeyword (k) => k match {

                case "constructor"| "function"| "method" => {
                    nextToken()
                    var ftype = parseType()
                    expectPeek(TIdentifier(null))
                    var fname = currToken match { case TIdentifier (v) => v}
                    expectPeek(TSymbol('('))
                    parseParameterList()
                    expectPeek(TSymbol(')'))
                    var body = parseSubroutineBody()
                    return  ast.Subroutine (k, ftype , fname, body) :: parseSubroutineDec()
                }

                case _ => Nil

            }
            case _ => Nil
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


        
    def parseSubroutineCall () : ast.Expression = {
      var id = currToken match { case TIdentifier(i) => i}  
      
      if (peekTokenIs(TSymbol('(')) ) {
        expectPeek(TSymbol('('))
        var args = parseExpressionList()
        expectPeek(TSymbol(')'))
        return ast.Call(id,args)
      } else {
        expectPeek(TSymbol('.'))
        expectPeek(TIdentifier(null))
        var fname = currToken match { case TIdentifier(i) => i} 
        expectPeek(TSymbol('('))
        var args = parseExpressionList()
        expectPeek(TSymbol(')'))
        return ast.Call(fname,args)
      }

    }


    def parseExpressionList () : List[ast.Expression] = {
        if (peekTokenIs(TSymbol(')'))) {
            return List.empty
        }else {
            var exp = parseExpression()  
            if (peekTokenIs(TSymbol(','))) {
                expectPeek(TSymbol(','))
                return exp :: parseExpressionList()
            } else {
                return exp :: parseExpressionList()
            }
        }
    }

    def parseTerm () : ast.Expression = {
        
        peekToken match {

            case TKeyword (tk) => {
                tk match {
                    case "false"| "true" | "null" |"this" => {
                        nextToken()
                        return ast.KeywordLiteral(tk)
                    }
                    case _ => throw new Exception ("sintax error")
                }
            }

            case TIntConst (value) => {
                nextToken()
                return ast.IntegerLiteral(value)
            }

            case TIdentifier(id) => {
                nextToken();
                if (peekTokenIs(TSymbol('(')) || peekTokenIs(TSymbol('.')) ) {
                    return parseSubroutineCall()
                } else if (peekTokenIs(TSymbol('['))){
                    expectPeek(TSymbol('['))
                    var exp = parseExpression()
                    expectPeek(TSymbol(']'))
                    return ast.IndexVariable(id,exp)

                } else {
                    return ast.Variable(id);
                }
                
            }

            case TSymbol(op) => {
                op match {
                    case '(' => {
                        expectPeek(TSymbol('(')) // avançar
                        var exp = parseExpression()
                        expectPeek(TSymbol(')')) // avançar 
                        return exp
                    }

                    case '-' | '~' => {
                        nextToken()
                        var exp = parseTerm()
                        return ast.UnaryExpression(op, exp)
                    }

                    case _ => {
                        throw new Exception ("sintax error "+op + currToken) 
                    }

                }
            }


        }
    }

  
   
    private def expectPeek (tk:Token)  = {
        tk match {
            case TIdentifier (_) => {
                peekToken match {
                    case TIdentifier (_) =>  nextToken()
                    case _ => throw new Exception ("sintax error")
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

    def peekTokenIs(token: Token) =  token == peekToken
    

    def isStatement (token :Token) : Boolean = {
        token match {
            case TKeyword ("if")| TKeyword ("let")
                 |TKeyword ("while") | TKeyword ("do") |TKeyword ("return")
                    => return true
            case _ => false
        }
    }
  
  
}
