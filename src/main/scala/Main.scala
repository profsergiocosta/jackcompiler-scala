package jackcompiler



@main def compile: Unit = 
  println("compiling")

  //val source = scala.io.Source.fromFile("/home/sergio/developing/Main.jack")
  
  val input = "\"ola\""  //(source.getLines mkString "\n")
/*
  val parser = JackParser(input)
  //val st = parser.parseClassVarDec()
  //val st = parser.parseClass()
  //val st = parser.parseStatement()
  val st = parser.parseExpression()
  //val st = parser.parseExpressionList()
  //var visitor = new AstPrinter()
  var visitor = VisitWriter()
  println(st)
  st.accept(visitor)
  println()
  print (visitor.vmOutput)*/
  

def msg = "I was compiled by Scala 3. :)"
