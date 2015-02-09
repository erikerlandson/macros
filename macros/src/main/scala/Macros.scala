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

  def treeSymbol(s: c.Tree): scala.Symbol = {
    val q"scala.Symbol.apply($stree)" = s
    val q"${sname: String}" = stree
    scala.Symbol(sname)
  }

  def breakOccurrences(tree: c.Tree): (Seq[scala.Symbol], Seq[scala.Symbol]) = {
    var breaks = List.empty[scala.Symbol]
    var breakables = List.empty[scala.Symbol]
    val tv = new Traverser {
      override def traverse(tr: Tree) {
        tr match {
          case q"$_.breakable($sym)" => { breakables = treeSymbol(sym) :: breakables }
          case q"$_.break($sym)" => { breaks = treeSymbol(sym) :: breaks }
          case _ => {
            super.traverse(tr)
          }
        }
      }
    }
    tv.traverse(tree)
    (breakables, breaks)
  }

  def traversal(b: c.Tree): c.Tree = {
    val (breakables, breaks) = breakOccurrences(b)
    println(s"breakables= $breakables")
    println(s"breaks= $breaks")
    // is there a standard way to signal compilation failure in macros?
    if (breakables.length != breakables.distinct.length) {
      throw new Exception("Duplicated breakable symbol name")
    }
    if ((breaks.toSet -- breakables.toSet).size > 0) {
      throw new Exception("Invoking break on undefined breakable symbol")
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
