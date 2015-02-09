object TypeString {
  import scala.reflect.runtime.universe._
 
  // return a human-readable type string for type argument 'T'
  // typeString[Int] returns "Int"
  def typeString[T :TypeTag]: String = {
    def work(t: Type): String = {
      t match { case TypeRef(pre, sym, args) =>
        val ss = sym.toString.stripPrefix("trait ").stripPrefix("class ").stripPrefix("type ")
        val as = args.map(work)
        if (ss.startsWith("Function")) {
          val arity = args.length - 1
          "(" + (as.take(arity).mkString(",")) + ")" + "=>" + as.drop(arity).head
        } else {
          if (args.length <= 0) ss else (ss + "[" + as.mkString(",") + "]")
        }
      }
    }
    work(typeOf[T])
  }
 
  // get the type string of an argument:
  // typeString(2) returns "Int"
  def typeString[T :TypeTag](x: T): String = typeString[T]
}
