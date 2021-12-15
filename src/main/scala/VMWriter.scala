package jackcompiler

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


class VMWriter extends ast.Visitor {
    def visitLetStatement (v: LetStatement) = {
        v.exp.accept(this)
        println("pop " +  v.id)
    }
    def visitExpression (v: Expression) = {

    }

    def visitVariable (v: Variable) = {
        //print(v.varName)
    }

    def visitIntegerLiteral (v: IntegerLiteral) = {
        println ("push const " + v.value)
    }

    def visitBinaryExpression (v: BinaryExpression) = {

        v.left.accept(this)      
        v.right.accept(this)
        println(vmOperator(v.operator))
    }

    def visitStatements (v: Statements) = {

    }

    def visitIfStatement (v: IfStatement)  = {
        
    }


    def visitWhileStatement (v: WhileStatement)  = {
        
    }

    def vmOperator (c:Char) : String = {
        c match {
            case '*' => "call Math.multiply 2"
            case '/' => "call Math.divide 2"
            case _  =>  return binOperators(c)
        }
        
    }    

    
}
