package jackcompiler

abstract class Token
case class TKeyword(s:String) extends Token
case class TSymbol (c:Char) extends Token
case class TIdentifier(s:String) extends Token
case class TStringConst(s:String) extends Token
case class TIntConst(i:Int) extends Token
case class TEOF (t:Char) extends Token
