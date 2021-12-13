package jackcompiler.ast

abstract class Visitor {
    def visitLetStatement (v: LetStatement) : Unit
    def visitExpression (v: Expression) : Unit
    def visitVariable (v: Variable) : Unit
    def visitIdentifier (v: Identifier) : Unit
    def visitLiteraInteger (v: IntegerLiteral) : Unit
}

abstract class Node {
    def accept(v: Visitor) :Unit
}

abstract class Expression extends Node
abstract class Statement extends Node
abstract class Identifier extends Expression
case class Variable (val varName:String) extends Identifier {
    def accept (v: Visitor) = {
        return v.visitVariable(this)
    }
}
case class IndexVariable (val varName: String, exp: Expression) extends Identifier{
    def accept (v: Visitor) = {
        
    }
}
case class IntegerLiteral (val value:Int) extends Expression {
    def accept (v: Visitor) = {
        return v.visitLiteraInteger(this)
    }
}
case class LetStatement (val id:Identifier, val exp:Expression) extends Statement{
    def accept (v: Visitor) = {
        return v.visitLetStatement(this)
    }
}


