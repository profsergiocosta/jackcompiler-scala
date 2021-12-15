package jackcompiler



@main def compile: Unit = 
  println("compiling")
  
  val parser = JackParser("/home/sergio/developing/Main.jack")
  val st = parser.parseStatements()
  var visitor = new AstPrinter()
  //var visitor = VMWriter()
  println(st.head)
  st.head.accept(visitor)
  println()
  
  

def msg = "I was compiled by Scala 3. :)"
