package fpinscala.state

import fpinscala.state.RNG.Simple

/**
  * Created by flori on 29/01/2017.
  */
object Practice {

  def main(args: Array[String]): Unit = {
    val rng = Simple(42)
    println(RNG.ints(5)(rng)._1)
    println(RNG.int(rng))
    println(RNG.doubleViaMap(rng)._1)
    println(RNG.randIntDouble(rng))
  }
}
