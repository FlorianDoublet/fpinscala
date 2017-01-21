package fpinscala.errorhandling

/**
  * Created by flori on 21/01/2017.
  */
object Practice {


  /**
    * Top secret formula for computing an annual car
    * insurance premium from two key factors.
    */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    numberOfSpeedingTickets / age
  }

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def parseInts(a: List[String]): Option[List[Int]] =
    Option.traverse(a)((x: String) => Try(x.toInt))

  def main(args: Array[String]): Unit = {
    println(parseInsuranceRateQuote("2", "6"))
    val listOpt = List(Some(1), Some(2), Some(3))
    println(Option.sequence(listOpt))
    println(Option.sequenceViaTraverse(listOpt))

    println(parseInts(List("1", "2", "3")))
  }

}
