
package jackcompiler

import scala.collection.mutable


case class Symbol(name: String, typeOf: String, kind: Kind.Kind, index: Int)

class SymbolTable {
  

  private val classScope: mutable.Map[String, Symbol] = mutable.Map()
  private val subroutineScope: mutable.Map[String, Symbol] = mutable.Map()
  private val countVars: mutable.Map[Kind.Kind, Int] = mutable.Map(
    Kind.ARG -> 0,
    Kind.VAR -> 0,
    Kind.STATIC -> 0,
    Kind.FIELD -> 0
  )

  def startSubroutine(): Unit = {
    subroutineScope.clear()
    countVars.update(Kind.ARG, 0)
    countVars.update(Kind.VAR, 0)
  }

  private def scope(kind: Kind.Kind): mutable.Map[String, Symbol] =
    if (kind == Kind.STATIC || kind == Kind.FIELD) classScope else subroutineScope

  def define(name: String, typeOf: String, kind: Kind.Kind): Unit = {
    val scopeTable = scope(kind)
    if (scopeTable.contains(name)) {
      throw new RuntimeException("Variable already defined")
    }

    val s = Symbol(name, typeOf, kind, varCount(kind))
    scopeTable(name) = s
    countVars.update(kind, countVars(kind) + 1)
  }

  def resolve(name: String): Option[Symbol] = {
    subroutineScope.get(name).orElse(classScope.get(name))
  }

  def varCount(kind: Kind.Kind): Int = {
    countVars(kind)
  }
}
