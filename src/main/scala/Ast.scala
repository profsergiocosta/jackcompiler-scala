package jackcompiler.ast

import jackcompiler.Token
import jackcompiler.Kind


// ------------------- Inicio da AST

abstract class Node {
    def accept(v: Visitor) :Unit
}

abstract class Expression extends Node 
abstract class Statement extends Node
abstract class Identifier extends Expression


case class VarDeclaration (val kind :Kind.Kind, val varType :String, val name :String  ) extends Node {
     def accept (v: Visitor) = {
            return v.visitVarDeclaration(this)
    }
}

case class ClassDec (name: String,  classVardecs :List[VarDeclaration], subroutineDecs :List[Subroutine]) extends Node {
    def accept (v: Visitor) = {
        return v.visitClassDec(this)

    }
}



case class Subroutine (modifier:String, funcType:String, name: String, params:List[VarDeclaration],vars:List[VarDeclaration], statements: Statements) extends Node {
    def accept (v: Visitor) = {
        return v.visitSubroutine(this)
    }
}





case class BinaryExpression (val left:Expression, val operator: Char, val right: Expression ) extends jackcompiler.ast.Expression {
    override def accept (v: Visitor) = {
        return v.visitBinaryExpression(this)
    }
}


case class UnaryExpression (val operator: Char, val right: Expression ) extends jackcompiler.ast.Expression {
    override def accept (v: Visitor) = {
        return v.visitUnaryExpression(this)
    }
}

case class Variable (val varName:String) extends Identifier  {
    
    override def accept (v: Visitor) = {
        return v.visitVariable(this)
    }
}

case class IndexVariable (val varName: String, exp: Expression) extends Identifier  {
    override def accept (v: Visitor) = {
        return v.visitIndexVariable(this)
    }
}
case class IntegerLiteral (val value:Int) extends Expression  {

    override def accept (v: Visitor) = {
        return v.visitIntegerLiteral(this)
    }

}

case class StringLiteral (val value:String) extends Expression  {

    override def accept (v: Visitor) = {
        return v.visitStringLiteral(this)
    }

}

case class KeywordLiteral (val value:String) extends Expression  {

    override def accept (v: Visitor) = {
        return v.visitKeywordLiteral(this)
    }

}

case class Call (val name:String, arguments:List[Expression]) extends Expression  {

    override def accept (v: Visitor) = {
        return v.visitCall(this)
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


case class DoStatement (val subroutine:Expression) extends Statement{
    def accept (v: Visitor) = {
        return v.visitDoStatement(this)
    }
}

// ------------------- fim da AST

abstract class Visitor {


    def visitClassDec(v:ClassDec) : Unit
    def visitSubroutine(v:Subroutine) : Unit
    

    def visitVarDeclaration(v:VarDeclaration) : Unit


    def visitStatements (v: Statements) : Unit
    def visitLetStatement (v: LetStatement) : Unit
    def visitIfStatement (v: IfStatement) : Unit
    def visitWhileStatement (v: WhileStatement) : Unit
    def visitReturnStatement (v: ReturnStatement) : Unit
    def visitDoStatement (v: DoStatement) : Unit


    def visitVariable (v: Variable) : Unit
    def visitIndexVariable (v: IndexVariable) : Unit
    def visitIntegerLiteral (v: IntegerLiteral) : Unit
    def visitStringLiteral (v: StringLiteral) : Unit
    def visitKeywordLiteral (v: KeywordLiteral) : Unit
    def visitBinaryExpression (v: BinaryExpression) : Unit
    def visitUnaryExpression (v: UnaryExpression) : Unit
    def visitCall (v: Call) : Unit
    
    

    
    
}