//> using scala "2.13.10"

import scala.collection.mutable
import scala.util.matching.Regex

object Exceptions {
  case class Method(
      className: String,
      methodName: String,
      methodTypes: Seq[Any],
      cause: Throwable
  ) extends Exception(
        s"""Class '$className' does not contain method '$methodName' that can accept arguments of types '${methodTypes
            .mkString(
              ", "
            )}'. (Did you misspell the method name or are using the wrong arguments?)""",
        cause
      )
}

/** A utility that can reflectively construct a Scala class, with parameters,
  * and then call a method on it (also with parameters).
  */
object ReflectiveBuilder extends App {

  /** Try to convert a string to a Scala type. */
  private def stringToAny(str: String): Any = {

    /* Something that looks like object creation, e.g., "Foo(42)" */
    val classPattern = "([a-zA-Z0-9_$.]+)\\((.*)\\)".r

    str match {
      case boolean if boolean.toBooleanOption.isDefined => boolean.toBoolean
      case integer if integer.toIntOption.isDefined     => integer.toInt
      case float if float.toDoubleOption.isDefined      => float.toDouble
      case classPattern(a, b) =>
        val arguments = b.split(',').map(stringToAny)
        Class.forName(a).getConstructors()(0).newInstance(arguments.toSeq: _*)
      case string => str
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
        obj =
          try {
            Some(stringToAny(value))
          } catch {
            case e: java.lang.ClassNotFoundException =>
              throw new java.lang.ClassNotFoundException(
                s"Unable to reflectively construct object '$value'. (Did you misspell it?)",
                e
              )
          }
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
        try {
          methodArguments.append(stringToAny(value))
        } catch {
          case e: java.lang.ClassNotFoundException =>
            throw new java.lang.ClassNotFoundException(
              s"Unable to reflectively construct argument '$value'. (Did you misspell it?)",
              e
            )
        }
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
    try {
      obj.get.getClass().getMethod(methodName.get, methodTypes.toSeq: _*)
    } catch {
      case e: java.lang.NoSuchMethodException =>
        throw new Exceptions.Method(
          obj.get.getClass().getName(),
          methodName.get,
          methodTypes.toSeq,
          e
        )
    }

  private val result = method.invoke(obj.get, methodArguments.toSeq: _*)

  println(result)
}
