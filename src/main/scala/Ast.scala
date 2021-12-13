package jackcompiler.ast

abstract class Visitor {
    def visitLetStatement (v: LetStatement) : Unit
    def visitExpression (v: Expression) : Unit
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
case class Variable (val varName:String) extends Identifier 
case class IndexVariable (val varName: String, exp: Expression) 
case class IntegerLiteral (val value:Int) extends Expression 

case class LetStatement (val id:Identifier, val exp:Expression) extends Statement{
    def accept (v: Visitor) = {
        return v.visitLetStatement(this)
    }
}


