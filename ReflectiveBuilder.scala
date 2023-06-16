//> using scala "2.13.10"

import scala.collection.mutable
import scala.util.matching.Regex

/** A utility that can reflectively construct a Scala class, with parameters,
  * and then call a method on it (also with parameters).
  */
object ReflectiveBuilder extends App {

  /** Try to convert a string to a Scala type. */
  private def stringToAny(str: String): Any = {

    val classPattern = "([a-zA-Z0-9_$.]+)\\((.+)\\)".r

    str match {
      case boolean @ ("false" | "true") => boolean.toBoolean
      case integer if integer.forall(_.isDigit) =>
        integer.toInt.asInstanceOf[Int]
      case classPattern(a, b) =>
        val arguments = b.split(',').map(stringToAny)
        Class.forName(a).getConstructors()(0).newInstance(arguments.toSeq: _*)
      case unknown =>
        throw new java.lang.IllegalArgumentException(
          s"Cannot handle input '$unknown'"
        )
    }
  }

  // Parse arguments.
  private var mutableArgs = args.toList
  private var obj: Option[Any] = None
  private var debug: Boolean = false
  private var methodName: Option[String] = None
  private val methodArguments: mutable.ArrayBuffer[Any] =
    mutable.ArrayBuffer.empty
  while (mutableArgs.size != 0) {
    mutableArgs = mutableArgs match {
      case "--object" :: value :: tail =>
        if (obj.isDefined)
          throw new java.lang.IllegalArgumentException(
            "--object can only be specified once"
          )
        obj = Some(stringToAny(value))
        tail
      case "--debug" :: tail =>
        debug = true
        tail
      case "--method" :: name :: tail =>
        if (methodName.isDefined)
          throw new java.lang.IllegalArgumentException(
            "--method can only be specified once"
          )
        methodName = Some(name)
        tail
      case "--argument" :: value :: tail =>
        methodArguments.append(stringToAny(value))
        tail
      case unknown :: _ =>
        throw new java.lang.IllegalArgumentException(
          s"Unknown option '$unknown'"
        )
      case Nil => ??? /* This case is not possible */
    }
  }

  // Validate arguments.
  if (obj.isEmpty)
    throw new java.lang.IllegalArgumentException(
      "one --object must be specified"
    )

  if (methodName.isEmpty)
    throw new java.lang.IllegalArgumentException(
      "one --method must be specified"
    )

  if (debug) {
    println(s"object: ${obj.get}")
    println(s"methodName: ${methodName.get}")
    println("methodArguments:")
    if (methodArguments.nonEmpty)
      print(
        methodArguments
          .map(a => s"$a: ${a.getClass()}")
          .mkString("  - ", "\n  - ", "\n")
      )
  }

  private val methodTypes = methodArguments.map {
    /* Integer needs to stay as an Int and not become a java.lang.Integer. */
    case _: Int     => classOf[Int]
    case _: Boolean => classOf[Boolean]
    case a          => a.getClass()
  }

  private val method =
    obj.get.getClass().getMethod(methodName.get, methodTypes.toSeq: _*)

  private val result = method.invoke(obj.get, methodArguments.toSeq: _*)

  println(result)
}
