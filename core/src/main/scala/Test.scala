object Demo extends App {
  import breakablecomprehension._

  val x = 3
  val y = 1
  val z = 4


  // breakable('inner) here works as expected
  println("using foreach")
  breakable {
    for {
      j <- 1 to 10 breakable('loop);
      if (j > z) break('loop);
      if (j != y);
      k <- 1 to j breakable('inner);
      l <- 1 to 2 breakable('foo)
    } {
      if (k == x) break('loop)
      println(s"${(j,k,l)}")
    }
  }


// breakable('inner) here causes a runtime crash
/*
  // current framework throws away recent map and flatmap results
  println("using yield")
  val r1 = breakable {
    for {
      j <- 1 to 10 breakable('loop);
      if (j > z) break('loop);
      if (j != y);
      k <- 1 to j breakable('inner)
    } yield {
      if (k == x) break('loop)
      println(s"${(j,k)}")
      (j, k)
    }
  }
  println(s"""yield result:\n${r1.toList.mkString("\n")}""")
*/

}
