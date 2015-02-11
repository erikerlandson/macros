import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class LBGMacros(val c: Context) {
  import c.universe._

  import TypeString._

  private def breakableBlockValid(blk: c.Tree): Boolean = {
    // a single 'for' comprehension is valid: either a single 'map' or 'foreach' call
    blk match {
      case q"$_.map[$_](..$_)" => true
      case q"$_.foreach[$_](..$_)" => true
      case _ => false
    }
  }

  def breakableBlock(blk: c.Tree): c.Tree = {
   println(showCode(blk))
   if (!breakableBlockValid(blk)) throw new Exception("Invalid breakable block: "+showCode(blk))
    q"""
      {}
    """
  }
}

object LabeledBreakableGenerator {
  import scala.language.implicitConversions

  type BreakableGenerator[+A] = BreakableIterator[A]

  // Generates a new breakable generator from any traversable object.
  def breakable[A](t1: TraversableOnce[A], label: Symbol): BreakableGenerator[A] =
    new BreakableIterator(t1.toIterator)

  def breakable[B](blk: => B): Unit = macro LBGMacros.breakableBlock

  // Mediates boolean expression with 'break' and 'continue' invocations
  case class BreakableGuardCondition(cond: Boolean) {
    def break(label: Symbol): Boolean = {
      false
    }
  }

  // implicit conversion of boolean values to breakable guard condition mediary
  implicit def toBreakableGuardCondition(cond: Boolean) =
    BreakableGuardCondition(cond)

  // An iterator that can be halted via its 'break' method.  Not invoked directly
  class BreakableIterator[+A](itr: Iterator[A]) extends Iterator[A] {
    private var broken = false
    def break { broken = true }

    def hasNext = !broken && itr.hasNext
    def next = itr.next
  }
}
