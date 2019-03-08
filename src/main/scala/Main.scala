

package JackCompiler

import java.io._

class JackCompiler {
  
  val comp = new CompilationEngine ("Main.jack")
  try {

    val pw = new PrintWriter(new File("Main.xml" ))
    pw.write(comp.compile)
    pw.close
  }  catch {
         case e: Exception =>{
            println (e)
            System.exit(0);
      }
}

}


object Main extends App{
  //new JackCompiler();
  val tok = new JackTokenizer ("Main.jack")
  tok.advance
  while (tok.hasMoreTokens) {
    println (tok.tagToken)
    tok.advance
  }

}