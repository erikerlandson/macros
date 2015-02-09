import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class MacrosImpl(val c: Context) {
  import TypeString._

  def hello: c.Tree = {
    import c.universe._
    q"""println("Hello World")"""
  }

  def throwable(s: c.Tree, v: c.Tree): c.Tree = {
    import c.universe._
    println(s"s= $s")
    println(s"c.ma= ${c.macroApplication}")
    val q"scala.Symbol.apply($stree)" = s
    println(s"stree= $stree  type= ${typeString(stree)}")
    val q"${sname: String}" = stree
    println(s"sname= $sname  type= ${typeString(sname)}")
    val sterm = TypeName(s"${sname}BreakThrowable")
    q"""
      {
        class $sterm extends scala.util.control.ControlThrowable
        try {
          if ($v > 10) {
            throw new $sterm
          }
        } catch {
          case bt: $sterm => {
            println("caught my type!")
          }
          case _ : Throwable => {
            println("caught unknown throwable type!")
          }
        }
      }
    """
  }


  def debug(x: c.Tree): c.Tree = {
    import c.universe._
    val q"$_.$xterm" = x
    val xname = xterm match { case TermName(n) => n }
    q"""println($xname + "= " + $x)"""
  }
}

object Macros {
  def hello: Unit = macro MacrosImpl.hello
  def throwable(s: Symbol, v: Int): Unit  = macro MacrosImpl.throwable
  def debug(x: Any): Unit = macro MacrosImpl.debug
}
