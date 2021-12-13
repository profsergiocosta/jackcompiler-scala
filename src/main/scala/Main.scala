package JackCompiler

@main def compile: Unit = 
  println("Hello world!")
  println(msg)
  val tokenizer = JackTokenizer("/home/sergio/developing/Main.jack")
  while (tokenizer.hasMoreTokens()) {
    println (tokenizer.nextToken())
  }

def msg = "I was compiled by Scala 3. :)"
