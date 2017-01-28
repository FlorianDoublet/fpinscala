package fpinscala.laziness

/**
  * Created by flori on 28/01/2017.
  */
object Practice {

  def main(args: Array[String]): Unit = {
    val s: Stream[Int] = Stream(1,2,3,4)
    println("*** toList ***")
    println(s.toListRec)
    println(s.toList)
    println("*** take ***")
    println(s.take(2).toList)
    println(s.takeViaUnfold(2).toList)
    println("*** drop ***")
    println(s.drop(2).toList)
    println("*** takeWhile ***")
    println(s.takeWhile(_ < 3).toList)
    println(s.takeWhileViaFoldRight(_ < 3).toList)
    println(s.takeWhileViaUnfold(_ < 3).toList)
    println("*** forAll ***")
    println(s.forAll(_ < 4)) // should be false
    println(s.forAll(_ < 5)) // should be true
    println("*** map ***")
    println(s.map(_ => 2).toList)
    println(s.mapViaUnfold(_ => 2).toList)
    println("*** filter ***")
    println(s.filter(_ != 2).toList)
    println("*** append ***")
    println(s.append(Stream(5)).toList)
    println("*** flatMap ***")
    println(s.flatMap(i => Stream(i, i)).toList)
    println("*** from ***")
    println(Stream.from(1).take(10).toList)
    println(Stream.fromViaUnfold(1).take(10).toList)
    println("*** fibs ***")
    println(Stream.fibs().take(10).toList)
    println(Stream.fibsViaUnfold().take(10).toList)
    println("*** constant ***")
    println(Stream.constantViaUnfold("bl").take(3).toList)
    println("*** zipWith ***")
    println(s.zipWith(s)(_ + _).toList)
    println("*** zipAll ***")
    println(s.zipAll(Stream(5,6)).toList)
    println(s.startsWith(Stream(1, 2)))
    println("*** tails ***")
    println(s.tails.toList)
    println(s.tailsSol.toList)
    println(s.tailsViaScanRight.toList)
    println("*** hasSubsequence ***")
    println(s.hasSubsequence(Stream(2,3))) //should be true
    println(s.hasSubsequence(Stream(2,4))) //should be false
    println("*** scanRight ***")
    println(s.take(3).scanRight(0)(_ + _).toList)
  }

}
