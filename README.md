This is a standalone utility for evaluating Scala code reflectively.  Think Python's [`eval()`](https://www.programiz.com/python-programming/methods/built-in/eval).

This works by using Java reflection to try to construct an object from a class
name that you give it.  It then executes a method of that class with provided
arguments.

Example usage:

```
# scala-cli ReflectiveBuilder.scala --main-class ReflectiveBuilder --extra-jars <some-scala-project>/target/scala-2.13/classes -- --object 'foo.bar.Baz' --method apply --argument 'foo.bar$Qux(42, true)'
```
