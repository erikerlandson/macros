object Demo extends App {
  import TypeString._
  val foo = 1
  Macros.debug(foo)
  Macros.throwable('eje, 5)
}
