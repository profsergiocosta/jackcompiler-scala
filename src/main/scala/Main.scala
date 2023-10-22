package jackcompiler

// /home/sergio/apps/nand2tetris/projects/11/Seven/Main.jack

import scala.io.Source
import java.io._


def removeFileExtension(fileName: String): String = {
  if (fileName.contains('.')) {
    fileName.stripSuffix("." + fileName.split('.').last)
  } else {
    fileName // Não havia extensão no nome do arquivo
  }
}

def compileFile (file: File) : Unit = {
      var fName = file.getPath

      println(s"compiling '$fName'")
      val source = Source.fromFile(file)
      val content = source.getLines().mkString("\n")
      val parser = JackParser(content)
      val st = parser.parseClass()
      var visitor = VisitWriter()
      st.accept(visitor)
     
      var outputFname = removeFileExtension(fName) + ".vm" 
      val writer = new PrintWriter(new File(outputFname))
      writer.write(visitor.vmOutput)
      writer.close()

}

@main def compile(fName: String): Unit = 
  
  
  val fileOrDir = new java.io.File(fName)
  if (fileOrDir.isDirectory()) {
    
    val extension = ".jack"
    
     val jackFiles = fileOrDir.listFiles().filter(file => file.isFile && file.getName.endsWith(extension))

    if (jackFiles.nonEmpty) {
      
      jackFiles.foreach(file => compileFile(file))
    } else {
      println(s"Nenhum arquivo com extensão.")
    }

  } else if (fileOrDir.isFile) {
    compileFile(fileOrDir)
  } else {
    println(s"O arquivo $fName não foi encontrado.")
  }

  
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
  


