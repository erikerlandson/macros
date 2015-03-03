import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class LBGMacros(val c: Context) {
  import c.universe._
  import TypeString._

  def isMonadicOp(op: TermName) = List("map", "flatMap", "foreach").contains(termString(op))
  def isNonUnitOp(op: TermName) = List("map", "flatMap").contains(termString(op))

  private def breakableBlockValid(blk: c.Tree): Boolean = {
    // a single 'for' comprehension is valid: either a single 'map' or 'foreach' call
    blk match {
      case q"$_.$op[$_]($_)" if (isMonadicOp(op)) => true
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

  def lbgCT(lab: String) = TypeName(s"${lab}$$ThrowableLBG")
  def lbgObj(lab: String) = TermName(s"${lab}$$ThrowableObj")
  def lbgThrow(lab: String) = TermName(s"${lab}Throw")
  def lbgBG(lab: String) = TermName(s"${lab}$$IterLBG")

  def xformSubLGE(expr: c.Tree, label: String): c.Tree = {
    val brkVar = lbgBG(label)
    expr match {
      case q"LabeledBreakableGenerator.breakable[$_]($_, $_)" => q"$brkVar"
      case q"""$sub.withFilter(
         $a => LabeledBreakableGenerator.toBreakableGuardCondition($p).break($labSym))""" => {
        val labStr = treeSymbol(labSym).toString.drop(1)
        val ss = xformSubLGE(sub, label)
        if (labStr == label) {
          q"""$ss.withFilter($a => {
            val r = $p
            if (r) $brkVar.break
            !r
          })"""
        } else {
          val ctThrow = lbgThrow(labStr)
          q"""$ss.withFilter($a => {
            val r = $p
            if (r) $ctThrow()
            !r
          })"""
        }
      }
      case q"$sub.withFilter($f)" => {
        val ss = xformSubLGE(sub, label)
        q"$ss.withFilter($f)"
      }
      case _ => expr
    }
  }

  def lgeGE(expr: c.Tree): c.Tree = {
    expr match {
      case q"LabeledBreakableGenerator.breakable[$_]($ge, $_)" => ge
      case q"$sub.withFilter($_)" => lgeGE(sub)
      case _ => throw new Exception("lgeGE: NO PATTERN MATCHED")
    }
  }

  def xformBreak(ex: c.Tree): c.Tree = {
    val xf = new Transformer {
      override def transform(expr: c.Tree) = {
        expr match {
          case q"LabeledBreakableGenerator.break($labSym)" => {
            println("BREAK")
            check(expr)
            val labStr = treeSymbol(labSym).toString.drop(1)
            val ctThrow = lbgThrow(labStr)
            val ctObj = lbgObj(labStr)
//            q"{ $ctThrow() }"
            q"{ throw $ctObj }"
          }
          case _ => {
//            println("CATCHALL")
//            check(expr)
            super.transform(expr)
          }
        }
      }
    }
//    println("************** xformBreak *****************")
//    check(ex)
//    println("******")
    val r = xf.transform(ex)
//    println("*******************************************")
    r
  }

  def xformLGE(expr: c.Tree): c.Tree = {
    val xxx = expr match {
      case q"$sub.$op[$_]($ga => $gbody)" if (isMonadicOp(op) && !lgeLabel(sub).isEmpty) => {
        val labStr = lgeLabel(sub).get.toString.drop(1)
        val brkType = lbgCT(labStr)
        val brkVar = lbgBG(labStr)
        val ctObj = lbgObj(labStr)
        val ctThrow = lbgThrow(labStr)
        val subXform = xformSubLGE(sub, labStr)
        val ge = lgeGE(sub)
        val gbx = xformLGE(gbody)
//        val gx = q"""$ga => $gbx"""
        val opExpr = if (isNonUnitOp(op)) {
          q"""$subXform.$op { $$v =>
            try {
              val g = $ga => $gbx
              Some(g($$v))
            } catch {
              case _: $brkType => {
                $brkVar.break
                None
              }
            }
          }.filter(!_.isEmpty).map(_.get)"""
        } else {
          q"""$subXform.foreach { $$v =>
            try {
              val g = $ga => $gbx
              g($$v)
            } catch {
              case _: $brkType => $brkVar.break
            }
          }"""
        }
        c.untypecheck(opExpr)
        q"""{
          class $brkType extends scala.util.control.ControlThrowable
          object $ctObj extends $brkType
          def $ctThrow() { throw $ctObj }
          val $brkVar = new LabeledBreakableGenerator.BreakableIterator($ge.toIterator)
          $opExpr
        }"""
      }

      case q"$sub.$op[$t]($g)" if (isMonadicOp(op)) => {
        val subXform = xformSubLGE(sub, "")
        val gx = xformLGE(g)
        c.untypecheck(q"$subXform.$op[$t]($gx)")
      }

      case _ => expr
    }
    xformBreak(xxx)
  }

  def check(expr: c.Tree): Unit = {
    println(s"CODE= ${showCode(expr)}")
//    println(s"TYPE= ${c.typecheck(expr).tpe.toString}")
  }

  def breakableBlock(blk: c.Tree): c.Tree = {
//    println(showCode(blk))
    if (!breakableBlockValid(blk)) throw new Exception("Invalid breakable block: "+showCode(blk))
    println("****** entering xformLGE ********")
    val t = xformLGE(blk)
//    c.typecheck(t)
    println("****** exited xformLGE *******")
    check(t)
    println("***** EXITING breakableBlock ********")
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
