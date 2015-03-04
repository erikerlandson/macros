object Demo extends App {
  import breakablecomprehension._

  val x = 3
  val y = 1
  val z = 4


  println("using foreach")
  breakable {
    for {
      j <- 1 to 10 breakable('loop);
      if (j > z) break('loop);
      if (j != y);
      k <- 1 to j
    } {
      if (k == x) break('loop)
      println(s"${(j,k)}")
    }
  }

  // current framework throws away recent map and flatmap results
  println("using yield")
  val r1 = breakable {
    for {
      j <- 1 to 10 breakable('loop);
      if (j > z) break('loop);
      if (j != y);
      k <- 1 to j
    } yield {
      if (k == x) break('loop)
      println(s"${(j,k)}")
      (j, k)
    }
  }
  println(s"""yield result:\n${r1.toList.mkString("\n")}""")


}
