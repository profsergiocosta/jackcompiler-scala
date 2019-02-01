package JackCompiler

class CompilationEngine (val fName:String) {

	val jt = new JackTokenizer (fName)
  
  def nextToken () = jt.advance


	def expected (tk:String):String = {
		jt.getTokenAsString  match  {
			case `tk`=> jt.tagToken
			case _ => { throw new Exception ("expected "+tk )}
		}
	}

    def compile () = compileLetStatement


    def compileLetStatement () : String =  {
      var s = tagNonTerminal("letStatement")
      nextToken
      s+=expected("let")
      nextToken
      s+=jt.tagToken        
      nextToken
      s+=expected("=");
      nextToken
      s+=compileExpression()
      s+= untagNonTerminal("letStatement")
      s+=expected(";")
      s
    }
    def compileExpression() :String = {
      var s = tagNonTerminal("expression") 
      s+=compileTerm
      s
    }

    def compileTerm () : String = {
      var s = "" 
      jt.getToken match {
        case TIntConst (_) | TStringConst (_) | TKeyword (_) => {s+=jt.tagToken} 
	      case TIdentifier(i) => {
	      	val id =jt.getToken
		      nextToken
		    //s+=jt.tagToken
	      }
        case _ => 

      }
     
      //s+= jt.tagTo(ken
      nextToken
      s
    }
    def compileClass () : String = {
        var s = ""
        s+= tagNonTerminal("class")
        nextToken
        s+=expected("class")
        nextToken
        s+=jt.tagToken // class identifier
        nextToken
        s+=expected("{")
        nextToken
        s+=compileClassVarDec
        s+=compileSubRoutine
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
            s+=untagNonTerminal("classVardec")
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
                s+= jt.tagToken
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
                compileSubroutineBody
              }
        }
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
    def compileSubroutineBody():String = {
        var s = tagNonTerminal("varDec");
        s+= expected ("var")
        nextToken
        s+=compileType
        return ""      
    }
    // private
    def write(s:String) = println (s)
    def tagNonTerminal (s:String)= ("<" +s + ">\n")
    def untagNonTerminal(s:String)=("</"+s+">\n")
}