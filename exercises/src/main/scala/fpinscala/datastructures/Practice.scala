package fpinscala.datastructures
import List.append
/**
  * Created by Florian on 14/01/2017.
  */
object Practice {

  def main(args: Array[String]): Unit = {
    // should print 3
    println(List.x)

    val l1 = List(1,2,3)
    val l2 = List(4, 5)

    val res = append(l1, l2)
    println(res)

    val x = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(x)
    println(List.length(x))



  }

}
