package jackcompiler.ast

abstract class Node
abstract class Expression extends Node
case class Identifier (val varName:String) extends Expression 


