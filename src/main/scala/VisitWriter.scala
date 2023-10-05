package jackcompiler

import scala.compat.Platform.EOL

import jackcompiler.ast.* 
import jackcompiler.Segment
import jackcompiler.Command




class VisitWriter extends ast.Visitor {


    var vmWriter = VMWriter()

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

    }

    def visitIfStatement (v: IfStatement)  = {
        
    }


    def visitWhileStatement (v: WhileStatement)  = {
        
    }
    
    def visitReturnStatement (v: ReturnStatement) = {
        val exp = v.value.getOrElse(IntegerLiteral(0))
        exp.accept(this)
        vmWriter.writeReturn()


    }



    
}
