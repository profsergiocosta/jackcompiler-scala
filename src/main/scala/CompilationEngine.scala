package JackCompiler

class CompilationEngine (val fName:String) {

	val jt = new JackTokenizer (fName)
  
  def nextToken () = {
    if (jt.hasMoreTokens ) 
        jt.advance
    else
      throw new Exception ("syntax error current token "+jt.getToken )
  }

	def expected (tk:String):String = {
		jt.getTokenAsString  match  {
			case `tk`=> jt.tagToken
			case _ => { throw new Exception ("expected: "+tk +" found: " + jt.currToken)   }
		}
	}
    def compile () : String = {
      nextToken
      compileClass
    }

  def compileClass () : String = {
        var s = ""
        s+= tagNonTerminal("class")
        s+=expected("class")
        nextToken
        s+=jt.tagToken // class identifier
        nextToken
        s+=expected("{")
        nextToken
        s+=compileClassVarDec
        s+=compileSubRoutine
        s+=expected("}")
        s+=untagNonTerminal("class")
        return s
    }

    def compileClassVarDec(): String = {
      var s = ""
      jt.getToken match {
        case TKeyword (k) => k match {
          case "field" | "static" => {
            s+= tagNonTerminal("classVarDec");
            s+= expected(k) // field and static
            nextToken
            s+=compileType
            nextToken
            s+= jt.tagToken() // identifier
            nextToken
            s+=compileListIdentifier
            s+=untagNonTerminal("classVarDec")
            nextToken
            s+=compileClassVarDec()
          }
          case _ => ""
        }
        case _ => ""
      }
      return s
    }
    //adicional
    def compileListIdentifier():String = {
      var s = ""
      jt.getToken match {
        case TIdentifier (k) => {
            s+=jt.tagToken
            nextToken
            s+=compileListIdentifier
          } 
        case TSymbol (',') => {
            s+=expected(",")
            nextToken
            s+=compileListIdentifier
        }  
        case TSymbol (';') => {
          s+=expected(";")
        }
        case _ => ""
      }
      return s
    }
    // adicional
    def compileType () = {
      jt.getToken match {
        case TKeyword (k) => k match {
               case "int"|"char"|"boolean"|"void" => expected(k) // todo: considerar que variaveis nao podem ser do tipo void
               case _ =>  new Exception ("erro "+jt.tagToken())

           }
        case TIdentifier (_) => jt.tagToken()
        case _ => "" 
      }
    }
    def compileSubRoutine() :String  ={
      var s = ""
      jt.getToken match {
        case TKeyword (k) => k match {
              case "constructor" | "function" | "method" => {
                s+= tagNonTerminal("subroutineDec");
                s+= jt.tagToken
                //println (jt.tagToken)
                nextToken
                s+=compileType
                nextToken
                s+=jt.tagToken // identifier
                nextToken
                s+=expected("(")
                nextToken
                s+=tagNonTerminal("parameterList")
                s+=compileParameterList
                s+=untagNonTerminal("parameterList")
                s+=expected(")")
                nextToken
                s+=compileSubroutineBody
                //println(jt.tagToken )
                s+= untagNonTerminal("subroutineDec");
                s+=compileSubRoutine
                
              }
              case _ => ""
        }
        case _ => ""
      }
      return s
    }
    def compileParameterList():String = {
      var s = ""
      jt.getToken match {
        case TSymbol (',') => {
          s+=expected(",")
          nextToken
          s+=compileParameterList
        }
        case TIdentifier(_)| TKeyword(_) => {
          s+=compileType
          nextToken
          s+=jt.tagToken // identifier
          nextToken
          s+=compileParameterList
        }
        case _ => ""
      }
      
      return s
    }

    //'{' varDec* statements '}'
    def compileSubroutineBody():String = {
        var s = tagNonTerminal("subroutineBody");
        s+= expected ("{")
        nextToken
        s+= compileVarDec
        s+= compileStatements
        s+= expected ("}")
        nextToken
        s+= untagNonTerminal("subroutineBody")
        return s   
    }

    // varDec: 'var' type varName (',' varName)* ';'
    def compileVarDec(): String = {
      var s = ""
      jt.getToken match {
        case TKeyword (k) => k match {
          case "var"  => {
            s+= tagNonTerminal("varDec");
            s+= expected(k) // field and static
            nextToken
            s+=compileType
            nextToken
            s+= jt.tagToken() // identifier
            nextToken
            s+=compileListIdentifier
            s+=untagNonTerminal("varDec")
            nextToken
            s+=compileVarDec()
          }
          case _ => ""
        }
        case _ => ""
      }
      return s
    }


    def compileStatement () : String = {
        var s = ""
        jt.getToken match { 
            case TKeyword ("let") => { s+= compileLetStatement; s+=compileStatement }
            case TKeyword ("if") => { s+= compileIfStatement ; s+=compileStatement }
            case TKeyword ("while") => { s+= compileWhileStatement ; s+=compileStatement }
            case TKeyword ("do") => { s+= compileDoStatement ; s+=compileStatement }
            case TKeyword ("return") => { s+= compileReturnStatement ; s+=compileStatement }
            case _ => ""
        }
        s
    }

    def compileStatements () : String = {
      var s = tagNonTerminal("statements")
      s+=compileStatement
      s += untagNonTerminal("statements")
      s
    }



    def compileLetStatement () : String =  {
      var s = tagNonTerminal("letStatement")
      s+=expected("let")
      nextToken
      s+=jt.tagToken        
      nextToken
      jt.getToken match { 
        case TSymbol ('[') => {
            s+=expected("[");
            nextToken();
            s+=compileExpression();
            s+=expected("]");
            nextToken();
        }
        case _ => {}
      }
      s+=expected("=");
      nextToken
      s+=compileExpression()
      s+=expected(";")
      nextToken
      s+= untagNonTerminal("letStatement")
      s
    }

  /*
      'if' '(' expression ')' '{' statements '}'
      ('else'  '{' statements '}'
  */
    def compileIfStatement () : String =  {
      var s = tagNonTerminal("ifStatement")
      s+=expected("if")
      nextToken
      s+=expected("(");
      nextToken
      s += compileExpression()
      s+=expected(")")
      nextToken
      s+=expected("{")
      nextToken
      s += compileStatements()
      s+=expected("}");  
      nextToken
      jt.getToken match { 
        case TKeyword ("else") => {
          s+=expected("else");  
          nextToken
          s+=expected("{")
          nextToken
          s += compileStatements()
          s+=expected("}"); 
          nextToken
        }
        case _ =>
      }
      s+= untagNonTerminal("ifStatement")
      s
    }

    def compileReturnStatement () : String = {
      var s = tagNonTerminal("returnStatement")
       s+=expected("return")
       nextToken

      jt.getToken match { 
        case TSymbol (';') => ""
        case _ => {
            // é uma expressao
            s += compileExpression
        }
      }

      s+=expected(";")
      nextToken
      s+= untagNonTerminal("returnStatement")
      s
    }

  // 'while' '(' expression ')' '{' statements '}'
   def compileWhileStatement () : String =  {
      var s = tagNonTerminal("whileStatement")
      s+=expected("while")
      nextToken
      s+=expected("(");
      nextToken
      s += compileExpression()
      nextToken
      s+=expected(")")
      nextToken
      s+=expected("{")
      nextToken
      s += compileStatements()
      s+=expected("}");  
      nextToken
      s+= untagNonTerminal("whileStatement")
      s
    }

    // 'do' subroutineCall ';'
    def compileDoStatement () : String =  {
        var s = tagNonTerminal("doStatement")
        s+=expected("do")
        nextToken
        s += compileSubroutineCall()
        s+=expected(";")
        nextToken
        s+= untagNonTerminal("doStatement")
        s
    }

/*
subroutineCall: subroutineName '(' expressiontList ')' 
| (className|varName) '.' subroutineName '(' expressiontList ')'
*/

  def compileSubroutineCall (id:Token) : String =  {
         var s = tagNonTerminal("subroutineCall")
         s+= jt.tagTokenBy (id)  // subroutineName
        jt.getToken match {
          case TSymbol ('(') => {
            //println(jt.tagToken)
            s+=expected("(")
            nextToken
            s+= tagNonTerminal("expressionList ")
            s+=compileExpressionList
            s+= untagNonTerminal("expressionList")
            s+=expected(")")
          }
          case TSymbol('.') => {
            s+=expected(".")
            nextToken
            s+=jt.tagToken  // subroutineName
            //fname = fname + "." + jt->currentToken; 
            nextToken
            s+=expected("(")
            nextToken
            s+= tagNonTerminal("expressionList ")
            s+=compileExpressionList
            s+= untagNonTerminal("expressionList")
            s+=expected(")")

          }
          case _ => ""
        }
         nextToken
         s += untagNonTerminal("subroutineCall")
         s
  }

    def compileSubroutineCall () : String =  {
         var tk = jt.getToken
         nextToken
         var s = compileSubroutineCall(tk)
         s
    }

    // expressionList: (expression (',' expression)* )?
    def compileExpressionList  ()  : String =  {
        var s = compileExpression
        jt.getToken match {
          case TSymbol (',') => {
            s+=expected(",")
            nextToken
            s+=compileExpressionList
          }
          case _ => ""
        }
        s
    }


    /// auxiliar function
    def compileSubTerms() :String = {
      var s = ""
      jt.getToken match {
         
         /*
         (ch == '+' || ch == '-' || ch == '*' || ch == '/'
        || ch == '&' || ch == '|' || ch == '<' || ch == '>'
        || ch == '=');
*/
        case TSymbol('+')| TSymbol('-') => {
          s+=jt.tagToken
          nextToken
          s+=compileTerm
          s+= compileSubTerms
        }
        case _ => ""
      }
      s
    }

    def compileExpression() :String = {
      var s = tagNonTerminal("expression") 
      s+=compileTerm
      s+=compileSubTerms
      s+= untagNonTerminal("expression") 
      s
    }

    def compileTerm () : String = {
      var s = tagNonTerminal("term") 
      jt.getToken match {
        case TIntConst (_) | TStringConst (_) | TKeyword (_) => {s+=jt.tagToken ; nextToken} 
	      case TIdentifier(i) => {
          val id = jt.getToken
          nextToken
          jt.getToken match {
            case TSymbol ('[') =>{
              s+=jt.tagTokenBy(id)
              s+=expected("[")
              nextToken
              s+=compileExpression
              s+=expected("]")
              nextToken();
            }
            case TSymbol ('(')|TSymbol ('.') =>{ 
               s+= compileSubroutineCall(id);
            }

            case _ => {
              s+=jt.tagTokenBy(id)
            }
            
          }
	      }
        case TSymbol ('(') =>{
            s+=jt.tagToken()
            nextToken();
            s+=compileExpression
            //println(jt.tagToken)
            s+=expected(")")
            nextToken();
        }
        case TSymbol ('-')| TSymbol ('~') =>{
            s+=jt.tagToken
            nextToken();
            s+= compileTerm;
        }

        case _ => 

      }
      s+= untagNonTerminal("term") 
      s
    }
    
    // private
    def write(s:String) = println (s)
    def tagNonTerminal (s:String)= ("<" +s + ">\n")
    def untagNonTerminal(s:String)=("</"+s+">\n")
}