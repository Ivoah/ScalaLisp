import net.ivoah.slippy
import net.ivoah.slippy.{Value => v, Keyword => kw}

class LispTest extends munit.FunSuite {
  import slippy.stdlib
  
  def parseTest(code: String, expected: String)(implicit loc: munit.Location) = test(s"parse $code == \"$expected\"") {
    val ast = slippy.parse(code)
    assertEquals(ast.toString, expected)
  }

  def evalTest(code: String, expected: slippy.Value)(implicit loc: munit.Location) = test(s"eval $code == $expected") {
    val result = slippy.eval(code)
    assertEquals(result, expected)
  }

  parseTest("(+ 9 3)", "(+ 9 3)")
  parseTest("""{:foo "bar" :baz "bob"}""", """{:foo "bar" :baz "bob"}""")
  parseTest("[1 2 3]", "[1 2 3]")
  parseTest("""
    :foo ; a comment!
  """, ":foo")
  parseTest("""
    :foo; a comment!
  """, ":foo")
  parseTest(":foo ; a comment", ":foo")
  parseTest(":foo; a comment", ":foo")

  evalTest("(+ 9 3)", v(12))
  evalTest("""{:foo "bar" :baz "bob"}""", v(Map(v(kw("foo")) -> v("bar"), v(kw("baz")) -> v("bob"))))
  evalTest("[1 2 3]", v(Seq(v(1), v(2), v(3))))
  evalTest("(let [foo 9 bar 8] (+ foo bar))", v(17))
}
