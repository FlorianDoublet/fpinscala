package MyModule

/**
  * Created by flori on 08/01/2017.
  */
object MyModule {

  def abs(n: Int) : Int = if (n < 0) -n else n

  private def formatAbs(x: Int) : String = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFac(x: Int) : String = {
    val msg = "the factorial value of %d is %d"
    msg.format(x, factorial(x))
  }

  def factorial(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int) : Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  def fib(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, x: Int, y: Int): Int = {
      if (n <= 1) x
      else go(n-1, y, x+y)
    }
    go(n, 0, 1)
  }

  def main(args: Array[String]) : Unit = {
    println(formatAbs(-87))
    println(formatFac(4))
    println(fib(4).toString)
  }

}
