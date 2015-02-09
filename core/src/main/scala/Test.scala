object Test extends App {
  import scala.util.control.ControlThrowable
  import TypeString._
  val foo = 1
  Macros.debug(foo)
  Macros.throwable('eje, 5)
}
