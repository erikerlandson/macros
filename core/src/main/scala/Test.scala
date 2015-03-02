object Demo extends App {
  import LabeledBreakableGenerator._

  // This fails with intimidating compiler crash, after the AST is given back to the compiler:
/*
  breakable {
    for {
      j <- breakable(1 to 10, 'loop);
      if (j > 4) break('loop);
      if (j != 1);
      k <- 1 to j
    } {
      if (k == 3) break('loop)
      println(s"j= $j  k= $k")
    }
  }
*/

  // This is just the showCode from the AST created above, but it compiles and works:
{
  class loopThrowableLBG extends scala.util.control.ControlThrowable;
  val loopIterLBG = new LabeledBreakableGenerator.BreakableIterator(scala.Predef.intWrapper(1).to(10).toIterator);
  loopIterLBG.withFilter(((j: Int) => {
  val r = j.>(4);
  if (r)
    loopIterLBG.break
  else
    ();
  r.`unary_!`
})).withFilter(((j: Int) => j.!=(1))).foreach(((vv) => try {
    val g = ((j: Int) => scala.Predef.intWrapper(1).to(j).foreach[Unit](((k: Int) => {
      if (k.==(3))
        throw new loopThrowableLBG()
      else
        ();
      scala.Predef.println(scala.StringContext.apply("j= ", "  k= ", "").s(j, k))
    })));
    g(vv)
  } catch {
    case ((_): loopThrowableLBG) => loopIterLBG.break
  }))
}

}
