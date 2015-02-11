object Demo extends App {
  import TypeString._
  val foo = 1
  //Macros.debug(foo)
  //Macros.throwable('eje, 5)

/*
  def breakable(s: Symbol) {}
  def break(s: Symbol) {}
  Macros.traversal({
    breakable('foo)
    break('foo)
    object Obj1 {
      breakable('goo)
      break('goo)
// macro will throw a compilation error:
//      break('nope)
//      breakable('foo)
    }
  })
*/

  import LabeledBreakableGenerator._
  breakable {
    for {
      j <- breakable(1 to 10, 'loop);
      if (j > 3) break('loop)
    } yield {
      1 + j
    }
  }
}
