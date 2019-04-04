

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
  new JackCompiler();
  

  /*
  // apenas para escrever os tokens
  val tok = new JackTokenizer ("Main.jack")
  val pw = new PrintWriter(new File("Main.xml" ))
  tok.advance
  pw.println("<tokens>")
  while (tok.hasMoreTokens) {
    pw.print (tok.tagToken)
    tok.advance
  }
  pw.print("</tokens>")
  pw.close
  */
}