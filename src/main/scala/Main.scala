

package JackCompiler


class JackCompiler {
  
  val comp = new CompilationEngine ("Main.jack")
   println (comp.compile)
}


object JackCompiler extends App{
  
  new JackCompiler();
}