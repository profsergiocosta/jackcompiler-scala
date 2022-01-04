package jackcompiler



@main def compile: Unit = 
  println("compiling")
  
  val parser = JackParser("/home/sergio/developing/Main.jack")
  //val st = parser.parseClassVarDec()
  val st = parser.parseClass()
  //val st = parser.parseExpressionList()
  //var visitor = new AstPrinter()
  //var visitor = VMWriter()
  println(st)
  //st.head.accept(visitor)
  println()
  
  

def msg = "I was compiled by Scala 3. :)"
