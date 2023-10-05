package jackcompiler

import scala.compat.Platform.EOL

import jackcompiler.ast.* 
import jackcompiler.Segment
import jackcompiler.Command




class VisitWriter extends ast.Visitor {


    var vmWriter = VMWriter()

    private var ifLabelNum = 0
    private var whileLabelNum = 0

    def vmOutput = vmWriter.vmOutputString

    def visitLetStatement (v: LetStatement) = {
        v.exp.accept(this)
        println("pop " +  v.id)
    }
    def visitExpression (v: Expression) = {

    }

    def visitDoStatement (v: DoStatement) = {
        
    }

    def visitVariable (v: Variable) = {
        //print(v.varName)
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
