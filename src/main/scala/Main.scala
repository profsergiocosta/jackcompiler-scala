package jackcompiler



@main def compile: Unit = 
  println("compiling")
  
  val parser = JackParser("/home/sergio/developing/Main.jack")
  val let = parser.parseLetStatement()
  println(let)
  

def msg = "I was compiled by Scala 3. :)"
