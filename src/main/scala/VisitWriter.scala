package jackcompiler

import scala.compat.Platform.EOL

import jackcompiler.ast.* 

val binOperators = Map (
        '+' -> "add", 
        '-' -> "sub",
        '&' -> "and",
        '|' -> "or",
        '>' -> "gt",
        '<' -> "lt",      
        '=' -> "eq"
)


class VisitWriter extends ast.Visitor {

    var vmOutput =  new StringBuilder ("")

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
        //vmOutput.append (s"push constant ${v.value}\r\n" )
        vmOutput.append ("push constant %d\n".format(10) )
        
    }

    def visitBinaryExpression (v: BinaryExpression) = {

        //v.left.accept(this)      
        //v.right.accept(this)
       // vmOutput.append (vmOperator(v.operator)+EOL)
    }

    def visitUnaryExpression (v: UnaryExpression) = {

 
    }

    def visitCall (v: Call) = {

    }

    def visitKeywordLiteral (v: KeywordLiteral) = {
         
     }

    def visitStatements (v: Statements) = {

    }

    def visitIfStatement (v: IfStatement)  = {
        
    }


    def visitWhileStatement (v: WhileStatement)  = {
        
    }
    
    def visitReturnStatement (v: ReturnStatement) = {

    }

    def vmOperator (c:Char) : String = {
        c match {
            case '*' => "call Math.multiply 2"
            case '/' => "call Math.divide 2"
            case _  =>  return binOperators(c)
        }
        
    }    

    
}
