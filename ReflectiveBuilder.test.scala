//> using test.dep org.scalatest::scalatest::3.2.16

package reflectiveSpec

import org.scalatest._
import flatspec._
import matchers._
import reflective.{Exceptions, ReflectiveBuilder}

object Objects {
  case class Foo(a: Int) {
    def apply(): String = s"a is $a"
    def apply(addend: Int): String = s"a + $addend is ${a + addend}"
    def invert(bool: Boolean): Boolean = !bool
    def invert(bar: Bar): Bar = Bar(invert(bar.bool))
    def echo(str: String): String = str
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
          "reflectiveSpec.Objects$Foo(42)",
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
          "reflectiveSpec.Objects$Foo(42)",
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
          "reflectiveSpec.Objects$Foo(42)",
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
          "reflectiveSpec.Objects$Foo(43)",
          "--method",
          "invert",
          "--argument",
          "reflectiveSpec.Objects$Bar(false)"
        )
      )
    } should contain("Bar(true)")
  }

  it should "work for methods with string arguments" in {
    getStdOutLines {
      ReflectiveBuilder.main(
        Array(
          "--object",
          "reflectiveSpec.Objects$Foo(43)",
          "--method",
          "echo",
          "--argument",
          "Hello world!"
        )
      )
    } should contain("Hello world!")
  }

  "Error behavior of ReflectiveBuilder" should "provide a good error message if the class isn't found" in {
    val exception =
      the[java.lang.ClassNotFoundException] thrownBy ReflectiveBuilder.main(
        Array(
          "--object",
          "Foo()",
          "--method",
          "bar"
        )
      )
    exception.getMessage should include(
      "Unable to reflectively construct object 'Foo()'. (Did you misspell it?)"
    )
  }

  it should "provide a good error message if the argument isn't found" in {
    val exception =
      the[java.lang.ClassNotFoundException] thrownBy ReflectiveBuilder.main(
        Array(
          "--object",
          "reflectiveSpec.Objects$Foo(43)",
          "--method",
          "invert",
          "--argument",
          "Baz()"
        )
      )
    exception.getMessage should include(
      "Unable to reflectively construct argument 'Baz()'. (Did you misspell it?)"
    )
  }

  it should "provide a good error message if the method isn't found" in {
    val exception =
      the[Exceptions.Method] thrownBy
        ReflectiveBuilder.main(
          Array(
            "--object",
            "reflectiveSpec.Objects$Foo(43)",
            "--method",
            "bar",
            "--argument",
            "42",
            "--argument",
            "false"
          )
        )
    exception.getMessage should include(
      "Class 'reflectiveSpec.Objects$Foo' does not contain method 'bar' that can accept arguments of types 'int, boolean'. (Did you misspell the method name or are using the wrong arguments?)"
    )
  }

}
