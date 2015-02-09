import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class MacrosImpl(val c: Context) {
  import c.universe._

  import TypeString._

  def hello: c.Tree = {
    q"""println("Hello World")"""
  }

  def debug(x: c.Tree): c.Tree = {
    val q"$_.$xterm" = x
    val xname = xterm match { case TermName(n) => n }
    q"""println($xname + "= " + $x)"""
  }

  def throwable(s: c.Tree, v: c.Tree): c.Tree = {
    val q"scala.Symbol.apply($stree)" = s
    val q"${sname: String}" = stree
    val stype = TypeName(s"${sname}BreakThrowable")
    q"""
      {
        class $stype extends scala.util.control.ControlThrowable
        try {
          if ($v > 10) {
            throw new $stype
          }
        } catch {
          case bt: $stype => {
            println("caught my type!")
          }
          case _ : Throwable => {
            println("caught unknown throwable type!")
          }
        }
      }
    """
  }

  def traversal(b: c.Tree): c.Tree = {
    val tv = new Traverser {
    }
    q"""
    {}
    """
  }

}

object Macros {
  def hello: Unit = macro MacrosImpl.hello
  def debug(x: Any): Unit = macro MacrosImpl.debug
  def throwable(s: Symbol, v: Int): Unit  = macro MacrosImpl.throwable
  def traversal(b: => Unit): Unit = macro MacrosImpl.traversal
}
