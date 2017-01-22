package fpinscala.errorhandling


/**
  * Created by flori on 22/01/2017.
  */
object PracticeEither {
  /**
    * Top secret formula for computing an annual car
    * insurance premium from two key factors.
    */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    numberOfSpeedingTickets / age
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception,Double] =
    for {
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    //success (Right(2.0))
    println(parseInsuranceRateQuote("1","2"))
    //stacktrace exception in Left
    println(parseInsuranceRateQuote("1","m"))

    //success
    println(mkPerson("coincoin", 42))
    //failure
    println(mkPerson("", 42))

  }

}
