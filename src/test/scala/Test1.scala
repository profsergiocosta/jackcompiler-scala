import org.junit.Test
import org.junit.Assert.*

import jackcompiler.*

class Test1:

  @Test
  def testInt(): Unit = {
    val input =
      """10
      """

    
    
    val parser = new JackParser(input)

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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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

    
    val parser = new JackParser(input)
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


  @Test
  def testSimpleFunctions(): Unit = {
    val input =
      """
        class Main {
 
                function int soma () {
                        return  30;
                 }
                
                 function void main () {
                        var int d;
                        return;
                  }
               }  
      """

    
    val parser = new JackParser(input)
    val st = parser.parseClass()
 
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.soma 0
push constant 30
return
function Main.main 1
push constant 0
return
"""
      
    assertEquals(expected, actual)
  }


  @Test
  def testSimpleFunctionWithVar(): Unit = {
    val input =
      """
        class Main {
                
                 function void funcao () {
                        var int a, b,c, d;
                        return d;
                  }
               }  
      """

    
    val parser = new JackParser(input)
    val st = parser.parseClass()
 
    var visitor = VisitWriter()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.funcao 1
push local 0
return
"""
      
    assertEquals(expected, actual)
  }



  /*      
      println ("expected")
      print (expected)
      println ("atual")
      println (actual)
      */