

package JackCompiler

import java.io._


case class Symbol (val t:String, val k:String, val i:Int)

class SymbolTable {
    var classScope = scala.collection.mutable.Map[String, Symbol]()
    var localScope = scala.collection.mutable.Map[String, Symbol]()

    var count = scala.collection.mutable.Map[String, Int]()
    count += ("FIELD" -> 0, "STATIC" -> 0, "ARG" -> 0, "VAR" -> 0)


    def define(name:String, sType: String, kind:String)  {
        if (kind ==  "STATIC" || kind == "FIELD") {
            classScope  += (name -> Symbol(name, kind, count(kind)))
        } else {
            localScope  += (name -> Symbol(name, kind, count(kind)))
        }
        count(kind) = count(kind)+1
    }

    def varCount(kind:String) = count(kind)
    
    def kindOf(name:String) : String =  {
        if (localScope contains name) {
            return localScope(name).k
        } else {
            return classScope(name).k
        }
        return "NotFound"
    }


}

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

  val vm = new VMWriter ("teste.vm")
  vm.writeLabel("ola");
  vm.close()
  


  /*
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