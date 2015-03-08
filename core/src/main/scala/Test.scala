/*
scala.Predef.intWrapper(0).until(5)
.flatMap[(Int, Int), scala.collection.immutable.IndexedSeq[(Int, Int)]](
    ((j: Int) => 
      scala.Option.option2Iterable[(Int, Int)](
        Demo.this.o.map[(Int, Int)](
          ((k: Int) => 
            scala.Tuple2.apply[Int, Int](j, k))
        )
      )
    )
  )
  (scala.collection.immutable.IndexedSeq.canBuildFrom[(Int, Int)])
*/
/*

gen.map(f), where f: A=>B
operator 'hat': f -> fhat: E[A]=>E[B], where E[A] == Either[CT,A]
EI(gen.toIterator).map(fhat).filter(_.isRight).map(_.right.get)
fhat(ahat:E[A]): E[B] = {
  if (ahat.isLeft) ahat
  else {
    try {
      a = ahat.right.get
      b = f(a)
      Right(b)
    } catch {
      case _: CT => Left(ctObj)
    }
  }
}
*/
/*
gen.flatMap(f), where f: A=>Iterable[B]
operator 'hat':  f -> fhat: E[A]=>EI[B], where EI[B] == Iterator[Either[CT,B]]
fhat(ahat:E[A]): EI[B] = {
  if (ahat.isLeft) List(ahat).iterator
  else {
    a = ahat.right.get
    b = 
  }
}
*/
object Demo extends App {
  import breakablecomprehension._
  import TypeString._

  val o: Option[Int] = Some(5)
  val r1 = for {
    k <- o;
    j <- Some(0 until k)
  } yield {
    j
  }
  println(s"r1= $r1   type= ${typeString(r1)}")

  
  val r2 = breakable {
  for {
    j <- 0 until 5 breakable 'outer;
    k <- o
  } yield {
    (j,k)
  }
  }.toList
  println(s"r2= $r2   type= ${typeString(r2)}")

  val x = 3
  val y = 1
  val z = 4

/*
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
*/

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
