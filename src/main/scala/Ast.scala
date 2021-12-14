package jackcompiler.ast

import jackcompiler.Token




abstract class Visitor {
    def visitLetStatement (v: LetStatement) : Unit
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

case class BinaryExpression (val left:Expression, val operator: Token, val right: Expression ) extends jackcompiler.ast.Expression {
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

case class LetStatement (val id:Identifier, val exp:Expression) extends Statement{
    def accept (v: Visitor) = {
        return v.visitLetStatement(this)
    }
}


