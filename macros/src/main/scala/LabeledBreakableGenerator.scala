import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class LBGMacros(val c: Context) {
  import c.universe._
  import TypeString._

  private def breakableBlockValid(blk: c.Tree): Boolean = {
    // a single 'for' comprehension is valid: either a single 'map' or 'foreach' call
    blk match {
      case q"$_.map[$_](..$_)" => true
      case q"$_.flatMap[$_](..$_)" => true
      case q"$_.foreach[$_](..$_)" => true
      case _ => false
    }
  }

  def termString(t: TermName) = t match { case TermName(n) => n }

  def treeSymbol(s: c.Tree): scala.Symbol = {
    val q"scala.Symbol.apply($stree)" = s
    val q"${sname: String}" = stree
    scala.Symbol(sname)
  }

  def lgeLabel(expr: c.Tree): Option[scala.Symbol] = {
    expr match {
      case q"LabeledBreakableGenerator.breakable[$_]($_, $labTree)" => Some(treeSymbol(labTree))
      case q"$sub.withFilter($_)" => lgeLabel(sub)
      case _ => None
    }
  }

  def xformSubLGE(expr: c.Tree, label: String): c.Tree = {
    val brkVar = TermName(s"${label}IterLGE")
    expr match {
      case q"LabeledBreakableGenerator.breakable[$_]($_, $_)" => q"$brkVar"
      case q"""$sub.withFilter(
         $a => LabeledBreakableGenerator.toBreakableGuardCondition($p).break($labSym))""" => {
        val labStr = treeSymbol(labSym).toString.drop(1)
        val ss = xformSubLGE(sub, label)
        println(s"$labStr  =?=  $label")
        if (labStr == label) {
          q"""$ss.withFilter($a => {
            val r = $p
            if (r) $brkVar.break
            !r
          })"""
        } else {
          val labType = TypeName(s"${labStr}ThrowableLGE")
          q"""$ss.withFilter($a => {
            val r = $p
            if (r) throw new $labType
            !r
          })"""
        }
      }
      case q"$sub.withFilter($f)" => {
        val ss = xformSubLGE(sub, label)
        q"$ss.withFilter($f)"
      }
      case _ => throw new Exception("xformSubLGE: NO PATTERN MATCHED")
    }
  }

  def lgeGE(expr: c.Tree): c.Tree = {
    expr match {
      case q"LabeledBreakableGenerator.breakable[$_]($ge, $_)" => ge
      case q"$sub.withFilter($_)" => lgeGE(sub)
      case _ => throw new Exception("lgeGE: NO PATTERN MATCHED")
    }
  }

  def xformLGE(expr: c.Tree): c.Tree = {
    expr match {
      case q"$sub.$op[$_]($g)"
          if (List("map","flatMap","foreach").contains(termString(op))
              && !lgeLabel(sub).isEmpty) => {
        val labStr = lgeLabel(sub).get.toString.drop(1)
        val brkType = TypeName(s"${labStr}ThrowableLGE")
        val brkVar = TermName(s"${labStr}IterLGE")
        val subXform = xformSubLGE(sub, labStr)
        val ge = lgeGE(sub)
        val opExpr = if (List("map","flatMap").contains(termString(op))) {
          q"""$subXform.$op { vv =>
            try {
              val g = $g
              Some(g(vv))
            } catch {
              case _: $brkType => {
                $brkVar.break
                None
              }
            }
          }.filter(x => !x.isEmpty).map(_.get)"""
        } else {
          q"""$subXform.foreach { vv =>
            try {
              val g = $g
              g(vv)
            } catch {
              case _: $brkType => $brkVar.break
            }
          }"""
        }
        q"""{
          class $brkType extends scala.util.control.ControlThrowable
          val $brkVar = new LabeledBreakableGenerator.BreakableIterator($ge.toIterator)
          $opExpr
        }"""
      }
      case _ => throw new Exception("xformLGE: NO PATTERN MATCHED")
    }
  }

  def check(expr: c.Tree): Unit = {
    println(s"CODE= ${showCode(expr)}")
    println(s"TYPE= ${c.typeCheck(expr).tpe.toString}")
  }

  def breakableBlock(blk: c.Tree): c.Tree = {
    println(showCode(blk))
    if (!breakableBlockValid(blk)) throw new Exception("Invalid breakable block: "+showCode(blk))
    val t = xformLGE(blk)
    t
  }
}

object LabeledBreakableGenerator {
  import scala.language.implicitConversions

  type BreakableGenerator[+A] = BreakableIterator[A]

  // Semantically, represents a breakable generator inside a 'breakable' block
  // Stub that can't be invoked directly, must be processed via macro
  def breakable[A](t1: TraversableOnce[A], label: Symbol): BreakableGenerator[A] = ???

  def breakable[B](blk: => B): B = macro LBGMacros.breakableBlock

  // represents breaking a labled generator
  def break(label: Symbol): Unit = ???

  // Mediates boolean expression with 'break' and 'continue' invocations
  case class BreakableGuardCondition(cond: Boolean) {
    // Semantically, represents breaking some labeled generator
    // This is a stub that cannot be invoked directly outside a macro call
    def break(label: Symbol): Boolean = ???
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
