import org.junit.Test
import org.junit.Assert.*

import jackcompiler.*

class Test1:


  @Test
  def testExpression(): Unit = {
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

