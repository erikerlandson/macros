import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class LBGMacros(val c: Context) {
  import c.universe._

  private def check(expr: c.Tree): Unit = {
    println(s"CODE= ${showCode(expr)}")
    try {
      println(s"TYPE= ${c.typecheck(expr).tpe.toString}")
    } catch {
      case _: Exception => println("TYPE= <NONE>")
    }
  }

  private def termString(t: TermName) = t match { case TermName(n) => n }

  private def isMonadicOp(op: TermName) = List("map", "flatMap", "foreach").contains(termString(op))
  private def isNonUnitOp(op: TermName) = List("map", "flatMap").contains(termString(op))

  private def symbolStr(s: c.Tree): String = {
    val q"scala.Symbol.apply($stree)" = s
    val q"${sname: String}" = stree
    sname
  }

  private def lbgCT(lab: String) = TypeName(s"${lab}$$ThrowableLBG")
  private def lbgObj(lab: String) = TermName(s"${lab}$$ThrowableObj")
  private def lbgBG(lab: String) = TermName(s"${lab}$$IterLBG")

  object xformBreak extends Transformer {
    override def transform(expr: c.Tree) = {
      expr match {
        case q"LabeledBreakableGenerator.break($labSym)" => {
          val ctObj = lbgObj(symbolStr(labSym))
          q"{ throw $ctObj }"
        }

        case q"""$sub.withFilter(
          $a => LabeledBreakableGenerator.toBreakableGuardCondition($p).break($labSym))""" => {
          val subx = this.transform(sub)
          val ctObj = lbgObj(symbolStr(labSym))
          q"""$subx.withFilter($a => {
            val r = $p
            if (r) { throw $ctObj }
            !r
          })"""
        }

        case _ => super.transform(expr)
      }
    }
  }

  private object xformMap extends Transformer {
    private def label(expr: c.Tree): Option[String] = {
      expr match {
        case q"LabeledBreakableGenerator.breakable[$_]($_, $labTree)" => Some(symbolStr(labTree))
        case q"$sub.withFilter($_)" => label(sub)
        case _ => None
      }
    }

    private def lbg(expr: c.Tree, bgLab: String): c.Tree = {
      val brkVar = lbgBG(bgLab)
      expr match {
        case q"LabeledBreakableGenerator.breakable[$_]($_, $_)" => q"$brkVar"

        case q"""$sub.withFilter(
            $a => LabeledBreakableGenerator.toBreakableGuardCondition($p).break($labSym))""" 
            if (bgLab == symbolStr(labSym)) => {
          val subx = lbg(sub, bgLab)
          q"""$subx.withFilter($a => {
            val r = $p
            if (r) $brkVar.break
            !r
          })"""
        }

        case q"$sub.withFilter($f)" => {
          val subx = lbg(sub, bgLab)
          q"$subx.withFilter($f)"
        }

        case _ => throw new Exception("lbg: NO PATTERN MATCHED")
      }
    }

    private def generator(expr: c.Tree): c.Tree = {
      expr match {
        case q"LabeledBreakableGenerator.breakable[$_]($ge, $_)" => ge
        case q"$sub.withFilter($_)" => generator(sub)
        case _ => throw new Exception("generator: NO PATTERN MATCHED")
      }
    }

    override def transform(expr: c.Tree) = {
      expr match {
        case q"$sub.$op[$_]($g)" if (isMonadicOp(op) && !label(sub).isEmpty) => {
          val labStr = label(sub).get
          val brkType = lbgCT(labStr)
          val brkVar = lbgBG(labStr)
          val ctObj = lbgObj(labStr)
          val subx = lbg(sub, labStr)
          val ge = generator(sub)
          val gx = this.transform(g)
          val opExpr = if (isNonUnitOp(op)) {
            q"""$subx.$op { $$v =>
              try {
                val $$g = $gx
                Some($$g($$v))
              } catch {
                case _: $brkType => {
                  $brkVar.break
                  None
                }
              }
            }.filter(!_.isEmpty).map(_.get)"""
          } else {
            q"""$subx.foreach { $$v =>
              try {
                val $$g = $gx
                $$g($$v)
              } catch {
                case _: $brkType => $brkVar.break
              }
            }"""
          }
          q"""{
            class $brkType extends scala.util.control.ControlThrowable
            object $ctObj extends $brkType
            val $brkVar = new LabeledBreakableGenerator.BreakableGenerator($ge.toIterator)
            $opExpr
          }"""
        }

        case _ => super.transform(expr)
      }
    }
  }

  private def breakableBlockValid(blk: c.Tree): Boolean = {
    // a single 'for' comprehension is valid: either a single 'map' or 'foreach' call
    blk match {
      case q"$_.$op[$_]($_)" if (isMonadicOp(op)) => true
      case _ => false
    }
  }

  def breakableBlock(blk: c.Tree): c.Tree = {
    if (!breakableBlockValid(blk)) throw new Exception("Invalid breakable block: "+showCode(blk))
    val mapx = xformMap.transform(blk)
    val r = xformBreak.transform(c.untypecheck(mapx))
    r
  }
}

object LabeledBreakableGenerator {
  import scala.language.implicitConversions

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
  class BreakableGenerator[+A](itr: Iterator[A]) extends Iterator[A] {
    private var broken = false
    def break { broken = true }

    def hasNext = !broken && itr.hasNext
    def next = itr.next
  }
}
