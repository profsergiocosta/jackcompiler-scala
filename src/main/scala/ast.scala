package jackcompiler.ast

abstract class Node
abstract class Expression extends Node
abstract class Statement extends Node
case class Identifier (val varName:String) extends Expression 
case class IntegerLiteral (val value:Int) extends Expression 
case class LetStatement (val id:Identifier, val exp:Expression) extends Statement


