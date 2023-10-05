package jackcompiler

object Segment extends Enumeration {
  type Segment = Value
  val CONST = Value("constant")
  val ARG = Value("argument")
  val LOCAL = Value("local")
  val STATIC = Value("static")
  val THIS = Value("this")
  val THAT = Value("that")
  val POINTER = Value("pointer")
  val TEMP = Value("temp")
}


object Command extends Enumeration {
    type Command = Value
    val ADD, SUB, NEG, EQ, GT, LT, AND, OR, NOT = Value
  }

class VMWriter {
  private val vmOutput = new StringBuilder()




  import Segment._
  import Command._

  def vmOutputString: String = vmOutput.toString()

  def writePush(segment: Segment, index: Int): Unit = {
    vmOutput.append(s"push ${segment.toString.toLowerCase} $index\n")
  }

  def writePop(segment: Segment, index: Int): Unit = {
    vmOutput.append(s"pop ${segment.toString.toLowerCase} $index\n")
  }

  def writeArithmetic(command: Command): Unit = {
    vmOutput.append(s"${command.toString.toLowerCase}\n")
  }

  def writeLabel(label: String): Unit = {
    vmOutput.append(s"label $label\n")
  }

  def writeGoto(label: String): Unit = {
    vmOutput.append(s"goto $label\n")
  }

  def writeIf(label: String): Unit = {
    vmOutput.append(s"if-goto $label\n")
  }

  def writeCall(name: String, nArgs: Int): Unit = {
    vmOutput.append(s"call $name $nArgs\n")
  }

  def writeFunction(name: String, nLocals: Int): Unit = {
    vmOutput.append(s"function $name $nLocals\n")
  }

  def writeReturn(): Unit = {
    vmOutput.append("return\n")
  }
}
