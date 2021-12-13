package jackcompiler



@main def compile: Unit = 
  println("ola mundo")
  
  val parser = JackParser("/home/sergio/developing/Main.jack")
  parser.parseLetStatement()
  println("ola mundo 2")
  

def msg = "I was compiled by Scala 3. :)"
