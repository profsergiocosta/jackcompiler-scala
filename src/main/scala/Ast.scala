package jackcompiler.ast

import jackcompiler.Token




abstract class Visitor {

    def visitStatements (v: Statements) : Unit
    def visitLetStatement (v: LetStatement) : Unit
    def visitIfStatement (v: IfStatement) : Unit
    def visitWhileStatement (v: WhileStatement) : Unit
    def visitReturnStatement (v: ReturnStatement) : Unit
    
    def visitExpression (v: Expression) : Unit
    def visitVariable (v: Variable) : Unit
    def visitIntegerLiteral (v: IntegerLiteral) : Unit
    def visitBinaryExpression (v: BinaryExpression) : Unit
    

    
    
}

abstract class Node {
    def accept(v: Visitor) :Unit
}

abstract class Expression extends Node {
     def accept (v: Visitor) = {
        return v.visitExpression(this)
    }
}
abstract class Statement extends Node
abstract class Identifier extends Expression

case class VarDeclaration (val kind :String, val varType :String, val name :String  ) extends Node {
     def accept (v: Visitor) = {
    
    }
}

case class BinaryExpression (val left:Expression, val operator: Char, val right: Expression ) extends jackcompiler.ast.Expression {
    override def accept (v: Visitor) = {
        return v.visitBinaryExpression(this)
    }
}

case class Variable (val varName:String) extends Identifier  {
    
    override def accept (v: Visitor) = {
        return v.visitVariable(this)
    }
}

case class IndexVariable (val varName: String, exp: Expression) extends Identifier 
case class IntegerLiteral (val value:Int) extends Expression  {

    override def accept (v: Visitor) = {
        return v.visitIntegerLiteral(this)
    }

}

case class Statements (val sts:List[Statement]) extends Node {

    override def accept (v: Visitor) = {
        return v.visitStatements(this)
    }

}

case class LetStatement (val id:Identifier, val exp:Expression) extends Statement{
    def accept (v: Visitor) = {
        return v.visitLetStatement(this)
    }
}


case class IfStatement (val condition:Expression, val thenBranch: Statements, val elseBranch:Option[Statements]=None) extends Statement{
    def accept (v: Visitor) = {
        return v.visitIfStatement(this)
    }
}


case class WhileStatement (val condition:Expression, val body: Statements) extends Statement{
    def accept (v: Visitor) = {
        return v.visitWhileStatement(this)
    }
}

case class ReturnStatement (val value:Option[Expression]=None) extends Statement{
    def accept (v: Visitor) = {
        return v.visitReturnStatement(this)
    }
}

