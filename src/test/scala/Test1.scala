import org.junit.Test
import org.junit.Assert.*

import jackcompiler.*

def imprime (str:String) : Unit = {
for (char <- str) {
  val asciiCode = char.toInt
  println(s"Caractere: $char | Código ASCII: $asciiCode")
}

}
class Test1:
/*
  @Test
  def testInt(): Unit = {
    val input =
      """10
      """

    
    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)

    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 10
"""
      assertEquals(expected, actual)
  }


  @Test
  def testSimpleExpression1(): Unit = {
    val input =
      """10+20
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 10
push constant 20
add
"""
      assertEquals(expected, actual)
  }


  @Test
  def testSimpleExpression2(): Unit = {
    val input =
      """10* 20
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 10
push constant 20
call Math.multiply 2
"""
      assertEquals(expected, actual)
  }


  @Test
  def testString(): Unit = {
    val input =
      """"OLA"
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 3
call String.new 1
push constant 79
call String.appendChar 2
push constant 76
call String.appendChar 2
push constant 65
call String.appendChar 2
"""

      assertEquals(expected, actual)
  }



  @Test
  def testFalse(): Unit = {
    val input =
      """false
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 0
"""
      assertEquals(expected, actual)
  }


    @Test
  def testTrue(): Unit = {
    val input =
      """true
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 0
not
"""
      assertEquals(expected, actual)
  }

    @Test
  def testThis(): Unit = {
    val input =
      """this
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push pointer 0
"""
      assertEquals(expected, actual)
  }

  @Test
  def testNeg(): Unit = {
    val input =
      """- 10
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseExpression()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 10
neg
"""
      assertEquals(expected, actual)
  }


  @Test
  def testReturn(): Unit = {
    val input =
      """return;
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseReturnStatement()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 0
return
"""

      assertEquals(expected, actual)
  }

   @Test
  def testReturnValue(): Unit = {
    val input =
      """return 10+20;
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseReturnStatement()
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 10
push constant 20
add
return
"""

      assertEquals(expected, actual)
  }

 

  @Test
  def testIf(): Unit = {
    val input =
      """ if (false) {
                return 10;
            } else {
                return 20;
            }
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseIfStatement()
    //println (st)
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 0
if-goto IF_TRUE0
goto IF_FALSE0
label IF_TRUE0
push constant 10
return
goto IF_END0
label IF_FALSE0
push constant 20
return
label IF_END0
"""
      assertEquals(expected, actual)
  }


  @Test
  def testWhile(): Unit = {
    val input =
      """while (false) {
                return 10;
            } 
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseStatement()
    println (st)
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """label WHILE_EXP0
push constant 0
not
if-goto WHILE_END0
push constant 10
return
goto WHILE_EXP0
label WHILE_END0
"""
      assertEquals(expected, expected)
  }

*/

  @Test
  def testClass(): Unit = {
    val input =
      """
      class Main {
                static int d;
                 function int funcao () {
                        
                        return d;
                  }
                
                } 
      """

    val symbolTable = SymbolTable()
    val parser = new JackParser(input,symbolTable)
    val st = parser.parseClass()
    println (st)
    var visitor = VisitWriter(symbolTable)
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """
    ..
"""
    println (":"+actual)
      //assertEquals(expected, expected)
  }


  /*      print (expected)
      print ("atual")
      print (actual)
      */