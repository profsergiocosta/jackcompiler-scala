

package JackCompiler

import java.io._

class JackCompiler {
  
  val comp = new CompilationEngine ("MainS.jack")
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


object JackCompiler extends App{
  
  new JackCompiler();
}