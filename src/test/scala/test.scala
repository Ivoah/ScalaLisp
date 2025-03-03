import net.ivoah.slippy

class LispTest extends munit.FunSuite {
  import slippy.stdlib
  
  def parseTest(code: String, expected: String)(implicit loc: munit.Location) = test(s"parse $code == \"$expected\"") {
    val ast = slippy.parse(code)
    assertEquals(ast.toString, expected)
  }

  def evalTest[T](code: String, expected: T)(implicit loc: munit.Location) = test(s"eval $code == $expected") {
    val result = slippy.eval(code)
    assertEquals(result, expected)
  }

  parseTest("(+ 9 3)", "(+ 9.0 3.0)")
  parseTest("""{:foo "bar" :baz "bob"}""", """{:foo "bar" :baz "bob"}""")
  parseTest("[1 2 3]", "[1.0 2.0 3.0]")

  evalTest("(+ 9 3)", 12.0)
  evalTest("""{:foo "bar" :baz "bob"}""", Map(slippy.Keyword("foo") -> "bar", slippy.Keyword("baz") -> "bob"))
  evalTest("[1 2 3]", Seq(1.0, 2.0, 3.0))
  evalTest("(let [foo 9 bar 8] (+ foo bar))", 17.0)
}
