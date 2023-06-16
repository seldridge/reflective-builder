//> using test.dep org.scalatest::scalatest::3.2.16

import org.scalatest._
import flatspec._
import matchers._

object Objects {
  case class Foo(a: Int) {
    def apply(): String = s"a is $a"
    def apply(addend: Int): String = s"a + $addend is ${a + addend}"
    def invert(bool: Boolean): Boolean = !bool
    def invert(bar: Bar): Bar = Bar(invert(bar.bool))
  }

  case class Bar(bool: Boolean)
}

class ReflectiveBuilderSpec extends AnyFlatSpec with should.Matchers {

  def getStdOutLines[A](thunk: => A): Array[String] = {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      thunk
    }
    stream.toString().split('\n')
  }

  "ReflectiveBuilder" should "work for methods without arguments" in {
    getStdOutLines {
      ReflectiveBuilder.main(
        Array(
          "--object",
          "Objects$Foo(42)",
          "--method",
          "apply"
        )
      )
    } should contain("a is 42")
  }

  it should "work for methods with integer arguments" in {
    getStdOutLines {
      ReflectiveBuilder.main(
        Array(
          "--object",
          "Objects$Foo(42)",
          "--method",
          "apply",
          "--argument",
          "1"
        )
      )
    } should contain("a + 1 is 43")
  }

  it should "work for methods with boolean arguments" in {
    getStdOutLines {
      ReflectiveBuilder.main(
        Array(
          "--object",
          "Objects$Foo(42)",
          "--method",
          "invert",
          "--argument",
          "true"
        )
      )
    } should contain("false")
  }

  it should "work for methods with object arguments" in {
    getStdOutLines {
      ReflectiveBuilder.main(
        Array(
          "--object",
          "Objects$Foo(43)",
          "--method",
          "invert",
          "--argument",
          "Objects$Bar(false)"
        )
      )
    } should contain("Bar(true)")
  }

}
