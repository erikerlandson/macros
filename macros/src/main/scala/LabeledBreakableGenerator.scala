import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class BreakableComprehensionMacros(val c: Context) {
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
        case q"breakablecomprehension.break($labSym)" => {
          val ctObj = lbgObj(symbolStr(labSym))
          q"{ throw $ctObj }"
        }

        case q"""$sub.withFilter(
          $a => breakablecomprehension.toBreakingGuard($p).break($labSym))""" => {
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
    private def label(expr: c.Tree): Option[(String, c.Tree)] = {
      expr match {
        case q"breakablecomprehension.toBreakableGenerator[$_]($ge).breakable($labTree)" =>
          Some((symbolStr(labTree), ge))
        case q"$sub.withFilter($_)" => label(sub)
        case _ => None
      }
    }

    private def lbg(expr: c.Tree, bgLab: String): c.Tree = {
      val brkVar = lbgBG(bgLab)
      expr match {
        case q"breakablecomprehension.toBreakableGenerator[$_]($_).breakable($_)" => q"$brkVar"

        case q"""$sub.withFilter(
            $a => breakablecomprehension.toBreakingGuard($p).break($labSym))""" 
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

    override def transform(expr: c.Tree) = {
      expr match {
        case q"$sub.$op[$_]($g)" if (isMonadicOp(op) && !label(sub).isEmpty) => {
          val (labStr, ge) = label(sub).get
          val brkType = lbgCT(labStr)
          val brkVar = lbgBG(labStr)
          val ctObj = lbgObj(labStr)
          val subx = lbg(sub, labStr)
          val gx = this.transform(g)
          val opExpr = if (termString(op) == "map") {
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
          } else if (termString(op) == "flatMap") {
            q"""$subx.$op { $$v =>
              try {
                val $$g = $gx
                $$g($$v)
              } catch {
                case _: $brkType => {
                  $brkVar.break
                  Iterator.empty
                }
              }
            }"""
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
          val r = q"""{
            class $brkType extends scala.util.control.ControlThrowable
            object $ctObj extends $brkType
            val $brkVar = new breakablecomprehension.BreakableIterator($ge.toIterator)
            $opExpr
          }"""
          r
//          val q = xformBreak.transform(c.untypecheck(r))
//          println("**********************")
//          check(q)
//          println("**********************")
//          q
        }

        case _ => super.transform(expr)
      }
    }
  }

  private def valid(blk: c.Tree): Boolean = {
    // a single 'for' comprehension is valid: either a single 'map' or 'foreach' call
    blk match {
      case q"$_.$op[$_]($_)" if (isMonadicOp(op)) => true
      case _ => false
    }
  }

  def xform(blk: c.Tree): c.Tree = {
    if (!valid(blk)) throw new Exception("Invalid breakable block: "+showCode(blk))
    val mapx = xformMap.transform(blk)
    val r = xformBreak.transform(c.untypecheck(mapx))
//    check(r)
    r
  }
}

object breakablecomprehension {
  import scala.language.implicitConversions

  def breakable[B](blk: => B): B = macro BreakableComprehensionMacros.xform

  // Semantically, represents a breakable generator inside a 'breakable' block
  // Stub that can't be invoked directly, must be processed via macro
  //def breakable[A](t1: TraversableOnce[A], label: Symbol): BreakableIterator[A] = ???

  // represents breaking a labled generator
  def break(label: Symbol): Unit = ???

  class BreakableGenerator[A](val gen: TraversableOnce[A]) extends AnyVal {
    def breakable(label: Symbol): BreakableIterator[A] = ???
  }
  implicit def toBreakableGenerator[A](gen: TraversableOnce[A]) = new BreakableGenerator(gen)

  // Mediates boolean expression with 'break' invocations
  class BreakingGuard(val cond: Boolean) extends AnyVal {
    // Semantically, represents breaking some labeled generator
    // This is a stub that cannot be invoked directly outside a macro call
    def break(label: Symbol): Boolean = ???
  }

  // implicit conversion of boolean values to breakable guard condition mediary
  implicit def toBreakingGuard(cond: Boolean) = new BreakingGuard(cond)

  // An iterator that can be halted via its 'break' method.  Not invoked directly
  class BreakableIterator[+A](itr: Iterator[A]) extends Iterator[A] {
    private var broken = false
    def break { broken = true }

    def hasNext = !broken && itr.hasNext
    def next = itr.next
  }
}
