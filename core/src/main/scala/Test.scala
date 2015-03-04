object Demo extends App {
  import breakablecomprehension._

  val x = 3
  val y = 1
  val z = 4
  breakable {
    for {
      j <- 1 to 10 breakable 'loop;
      if (j > z) break('loop);
      if (j != y);
      k <- 1 to j breakable 'inner;
      if (k == 3) break('loop)
    } {
      if (k == x) break('loop)
      println(s"j= $j  k= $k")
    }
  }


}
