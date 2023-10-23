import org.junit.Test
import org.junit.Assert.*

import jackcompiler.*

class Test1:

  @Test
  def testInt(): Unit = {
    val input =
      """10
      """

    
    
    val parser = new Parser(input)

    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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
  def testSimpleExpression3(): Unit = {
    val input =
      """10 * 20 * 30
      """

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """push constant 10
push constant 20
call Math.multiply 2
push constant 30
call Math.multiply 2
"""


      assertEquals(expected, actual)
  }



  @Test
  def testString(): Unit = {
    val input =
      """"OLA"
      """

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseExpression()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseReturnStatement()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseReturnStatement()
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseIfStatement()
    //println (st)
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseStatement()
    //println (st)
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseClass()
 
    var visitor = CodeGenerator()
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

    
    val parser = new Parser(input)
    val st = parser.parseClass()
 
    var visitor = CodeGenerator()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.funcao 4
push local 3
return
"""
      assertEquals(expected, actual)
  }


  @Test
  def testLet(): Unit = {
    val input =
      """
            class Main {
            
              function void main () {
                  var int x;
                  let x = 42;
                  return;
              }
            } 
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()
 
    var visitor = CodeGenerator()
    st.accept(visitor)
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.main 1
push constant 42
pop local 0
push constant 0
return
"""


      assertEquals(expected, actual)
  }

  

  @Test
  def testLet2(): Unit = {
    val input =
      """
      class Main {

            static int y;
            
              function void main () {
                var int x;
                let x = 42+y;
                return x;
              }
            }
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()

    var visitor = CodeGenerator()
    st.accept(visitor)
   
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.main 1
push constant 42
push static 0
add
pop local 0
push local 0
return
"""

      
      assertEquals(expected, actual)
      
  }


  @Test
  def testArray(): Unit = {
    val input =
      """
            class Main {
                function void main () {
                    var Array v;
                    let v[2] = v[3] + 42;
                    return;
                }
            }
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()

    var visitor = CodeGenerator()
    st.accept(visitor)
   
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.main 1
push constant 2
push local 0
add
push constant 3
push local 0
add
pop pointer 1
push that 0
push constant 42
add
pop temp 0
pop pointer 1
push temp 0
pop that 0
push constant 0
return
"""
      assertEquals(expected, actual)

  }


  @Test
  def testCallFunction(): Unit = {
    val input =
      """
  class Main {
                function int soma (int x, int y) {
                       return  x + y;
                }
               
                function void main () {
                       var int d;
                       let d = Main.soma(4,5);
                       return;
                 }
               
               }
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()
    print (st)

    var visitor = CodeGenerator()
    st.accept(visitor)
   
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.soma 0
push argument 0
push argument 1
add
return
function Main.main 1
push constant 4
push constant 5
call Main.soma 2
pop local 0
push constant 0
return
"""
      assertEquals(expected, actual)
  }


  @Test
  def testCallMethod(): Unit = {
    val input =
      """
  class Main {
                field Point p;
 
                function void main () {
                       var int d;
                       let d = p.getX();
                       return;
                 }
               
               }
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()


    var visitor = CodeGenerator()
    st.accept(visitor)
   
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.main 1
push this 0
call Point.getX 1
pop local 0
push constant 0
return
"""


    assertEquals(expected, actual)
  }


  @Test
  def testDoStatement(): Unit = {
    val input =
      """
            class Main {
                function void main () {
                    var int x;
                    let x = 10;
                    do Output.printInt(x);
                    return;
                }
            }
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()
    print (st)

    var visitor = CodeGenerator()
    st.accept(visitor)
   
    val actual = visitor.vmOutput.toString
    val expected =
    """function Main.main 1
push constant 10
pop local 0
push local 0
call Output.printInt 1
pop temp 0
push constant 0
return
"""
   assertEquals(expected, actual)
}

 
  @Test
  def testMethodsConstructor(): Unit = {
    val input =
      """
            class Point {
                field int x, y;
            
                method int getX () {
                    return x;
                }
            
                method int getY () {
                    return y;
                }
            
                method void print () {
                    do Output.printInt(getX());
                    do Output.printInt(getY());
                    return;
                }
            
                constructor Point new(int Ax, int Ay) { 
                  var int w;             
                  let x = Ax;
                  let y = Ay;
                  let w = 42;
                  let x = w;
                  return this;
               }
              }
      """

    
    val parser = new Parser(input)
    val st = parser.parseClass()

    var visitor = CodeGenerator()
    st.accept(visitor)
   
    val actual = visitor.vmOutput.toString
    val expected =
    """function Point.getX 0
push argument 0
pop pointer 0
push this 0
return
function Point.getY 0
push argument 0
pop pointer 0
push this 1
return
function Point.print 0
push argument 0
pop pointer 0
push pointer 0
call Point.getX 1
call Output.printInt 1
pop temp 0
push pointer 0
call Point.getY 1
call Output.printInt 1
pop temp 0
push constant 0
return
function Point.new 1
push constant 2
call Memory.alloc 1
pop pointer 0
push argument 0
pop this 0
push argument 1
pop this 1
push constant 42
pop local 0
push local 0
pop this 0
push pointer 0
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