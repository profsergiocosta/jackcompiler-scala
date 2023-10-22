package jackcompiler

import scala.compat.Platform.EOL

import jackcompiler.ast.* 




class VisitWriter()  extends ast.Visitor {


    var vmWriter = VMWriter()

    private var ifLabelNum = 0
    private var whileLabelNum = 0
    private val symbolTable= SymbolTable ()
    private var className = ""

    def vmOutput = vmWriter.vmOutputString

    val kindToSegment = Map(
        Kind.STATIC -> Segment.STATIC,
        Kind.FIELD  -> Segment.THIS,
        Kind.VAR    -> Segment.LOCAL,
        Kind.ARG    -> Segment.ARG
    )



    def visitClassDec(v:ClassDec) : Unit = {
        className = v.name;
        v.classVardecs.foreach { st =>  st.accept (this) }
        v.subroutineDecs.foreach { st =>  st.accept (this) }
    }

    def visitVarDeclaration(v:VarDeclaration) : Unit = {
        symbolTable.define(v.name, v.varType, v.kind)
    
    }
    
    def visitSubroutine(v:Subroutine) : Unit= {

        ifLabelNum = 0
        whileLabelNum = 0
        
        symbolTable.startSubroutine()

        if (v.modifier == "method") 
            symbolTable.define("this", className, Kind.ARG);

        v.params.foreach  { vardec =>
                vardec.accept(this)
        }

        v.vars.foreach { vardec =>
                vardec.accept(this)
        }

        var nlocals = symbolTable.varCount(Kind.VAR);
        var funcName = className + "." + v.name
        vmWriter.writeFunction(funcName, nlocals)

        v.modifier match {
            case "constructor" => {
                vmWriter.writePush(Segment.CONST, symbolTable.varCount(Kind.FIELD))
                vmWriter.writeCall("Memory.alloc", 1)
                vmWriter.writePop(Segment.POINTER, 0)
            }
            case "method"  => {
                vmWriter.writePush(Segment.ARG, 0)
                vmWriter.writePop(Segment.POINTER, 0)
            }

            case _ => {}
        }

        v.statements.accept(this)

    }
    
 


    def visitLetStatement (v: LetStatement) = {

        v.id match {
            case Variable(varName) => {

                v.exp.accept(this)

                symbolTable.resolve(varName) match {
                    case Some (sym) => vmWriter.writePop(kindToSegment(sym.kind), sym.index)
                    case None => println ("variavel nao encontrada") // criar uma exceção
                }

            }
            case IndexVariable(varName, exp) => {

                exp.accept(this)

                symbolTable.resolve(varName) match {
                    case Some (sym) => vmWriter.writePush(kindToSegment(sym.kind), sym.index)
                    case None => println ("variavel nao encontrada") // criar uma exceção
                }               

                vmWriter.writeArithmetic(Command.ADD)
                v.exp.accept(this)

                // let arr[expression1] = expression2
                vmWriter.writePop(Segment.TEMP, 0);    // salva a expression2 
                vmWriter.writePop(Segment.POINTER, 1); // pop arr[expression1] into pointer 1
                vmWriter.writePush(Segment.TEMP, 0);   // push result back onto stack
                vmWriter.writePop(Segment.THAT, 0);    // Store right hand side evaluation in THAT 0.


            }
        }          

    }

    def visitDoStatement (v: DoStatement) = {
        v.subroutine.accept(this)
        vmWriter.writePop(Segment.TEMP, 0);
    }

    def visitVariable (v: Variable) = {
        
        var symOption = symbolTable.resolve(v.varName);

        symOption match {
            case Some (sym) => vmWriter.writePush(kindToSegment(sym.kind), sym.index)
            case None => println ("variavel nao encontrada") // criar uma exceção
        }
        
        
    }
    def visitIndexVariable (v: IndexVariable) : Unit = {
        v.exp.accept(this)

        symbolTable.resolve(v.varName) match { // todo: extrair esse tratamento
            case Some (sym) => vmWriter.writePush(kindToSegment(sym.kind), sym.index)
            case None => println ("variavel nao encontrada") // criar uma exceção
        }

        vmWriter.writeArithmetic(Command.ADD)
        vmWriter.writePop(Segment.POINTER,1)
        vmWriter.writePush(Segment.THAT,0)

    }

    def visitIntegerLiteral (v: IntegerLiteral) = {
        vmWriter.writePush(Segment.CONST,v.value)
        
    }

    def visitStringLiteral (v: StringLiteral) = {
        
        vmWriter.writePush(Segment.CONST, v.value.size)
        vmWriter.writeCall("String.new", 1);

        v.value.foreach { caracter =>
            vmWriter.writePush(Segment.CONST, caracter);
            vmWriter.writeCall("String.appendChar", 2);
        }
        
    }

    def visitBinaryExpression (v: BinaryExpression) = {

        v.left.accept(this)      
        v.right.accept(this)
        v.operator match {
            case '*' => vmWriter.writeCall ("Math.multiply",2)
            case '/' => vmWriter.writeCall ("Math.divide", 2)
            case '+' => vmWriter.writeArithmetic(Command.ADD) 
            case '-' => vmWriter.writeArithmetic(Command.SUB) 
            case '>' => vmWriter.writeArithmetic(Command.GT) 
            case '<' => vmWriter.writeArithmetic(Command.LT) 
            case '=' => vmWriter.writeArithmetic(Command.EQ) 
            case '&' => vmWriter.writeArithmetic(Command.AND) 
            case '|' => vmWriter.writeArithmetic(Command.OR) 
        }
    }

    def visitUnaryExpression (v: UnaryExpression) = {
        v.right.accept(this)
        v.operator match {
            case '~' => vmWriter.writeArithmetic(Command.NOT) 
            case '-' => vmWriter.writeArithmetic(Command.NEG)         
        }
    }

    def visitCall (v: Call) = {
        var nargs = v.arguments.size

        val parts = v.name.split("\\.") 

        var funcName = v.name

        if (parts.length == 1) { // metodo da propria classe
                vmWriter.writePush(Segment.POINTER, 0) // ponteiro this
                funcName = className + "." + v.name
                nargs+=1
                
        } else { // pode ser um metodo de um outro objeto ou uma função

                symbolTable.resolve(parts(0)) match { 
                case Some (sym) =>  {
                    // é um metodo de outro objeto
                    funcName = sym.typeOf + "." + parts(1)
                    vmWriter.writePush(kindToSegment(sym.kind), sym.index)
                    nargs+=1
                }
                case None => {} // é uma funcao
            }

        }

        v.arguments.foreach {
            exp => exp.accept(this)
        } 

        vmWriter.writeCall(funcName, nargs);
        
    }

    def visitKeywordLiteral (v: KeywordLiteral) = {

        v.value match {
            case "false" | "null" => vmWriter.writePush(Segment.CONST,0)
            case "true" => {
                vmWriter.writePush(Segment.CONST,0)
                vmWriter.writeArithmetic(Command.NOT)
            }
            case "this" => vmWriter.writePush(Segment.POINTER,0)
        }
         
     }

    def visitStatements (v: Statements) = {

        v.sts.foreach { st =>  st.accept (this) }

    }

    def visitIfStatement (v: IfStatement)  = {

        var labelTrue = "IF_TRUE" + ifLabelNum;
        var labelFalse = "IF_FALSE" + ifLabelNum;
        var labelEnd = "IF_END" + ifLabelNum;

        ifLabelNum += 1;
        v.condition.accept(this)
        
        vmWriter.writeIf(labelTrue);
        vmWriter.writeGoto(labelFalse);
        vmWriter.writeLabel(labelTrue);

        v.thenBranch.accept(this)

        if (v.elseBranch.isDefined)  vmWriter.writeGoto(labelEnd)

        vmWriter.writeLabel(labelFalse);

        for (st <- v.elseBranch)  {
            st.accept(this)
            vmWriter.writeLabel(labelEnd)
        }
            
    }


    def visitWhileStatement (v: WhileStatement)  = {
        var labelTrue = "WHILE_EXP" + whileLabelNum;
        var labelFalse = "WHILE_END" + whileLabelNum;
        whileLabelNum += 1 ;

        vmWriter.writeLabel(labelTrue);
        
        v.condition.accept(this)
        vmWriter.writeArithmetic(Command.NOT);
        vmWriter.writeIf(labelFalse);

        v.body.accept(this)

        vmWriter.writeGoto(labelTrue); // Go back to labelTrue and check condition
        vmWriter.writeLabel(labelFalse); // Breaks out of while loop because ~(condition) is true


    }
    
    def visitReturnStatement (v: ReturnStatement) = {
        val exp = v.value.getOrElse(IntegerLiteral(0))
        exp.accept(this)
        vmWriter.writeReturn()


    }



    
}
