package fpinscala.datastructures
import List.append
/**
  * Created by Florian on 14/01/2017.
  */
object Practice {

  def main(args: Array[String]): Unit = {
    // should print 3
    /*println(List.x)

    val l1 = List(1,2,3)
    val l2 = List(4, 5)

    val res = append(l1, l2)
    println(res)

    val x = List.foldRight(l1, Nil:List[Int])(Cons(_,_))
    println(x)
    println(List.length(x))

    val sum = List.sumFoldLeft(l1)
    // should print 6
    println("should be 6 : " + sum)

    val prod = List.productFoldLeft(List(1.0, 2.0, 3.0))
    println("should be 6.0 : " + prod)

    val lenght = List.lenghtFoldLeft(l1)
    println("should be 3 : " + lenght)

    val reverse = List.reverse(l1)
    println(" should be 3,2,1 : " + reverse)

    val listAfterAppend = List.appendFoldRight(l1, List(4))
    println("should be 1,2,3,4 : " + listAfterAppend)

    val concatList = List.concat(List(List(1), List(2)))
    println("should be 1,2 : " + concatList)

    val add1 = List.addOne(l1)
    println("should be 2,3,4 : " + add1)

    val doubleToString = List.doubleToString(List(1.0,2.0))
    println("should be 1.0, 2.0 : " + doubleToString)

    val map = List.map_without_stack(l1)(x => x * 2)
    println("shoudl be 2, 4, 6 : " + map)

    val filter = List.filter(l1)(x => x == 2)
    println("should be 1, 3 : " + filter)

    val flatmap = List.flatMap(List(1,2,3))(i => List(i,i))
    println("should be List(1,1,2,2,3,3) : " + flatmap )

    val filterFlatmap = List.filterViaFlatmap(l1)(x => x == 2)
    println("should be 1, 3 : " + filterFlatmap)

    val addingElement = List.addingElements(l1, List(4,5,6))
    println("should be 5,7,9 : " + addingElement)

    val addingElementZip = List.zipWith(l1, List(4,5,6))((x,y) => (x + y).toString )
    println("should be 5,7,9 : " + addingElementZip)

    println("**** test hasSubsequence ****\n")

    println("should be true : " + List.hasSubsequence(List(1,2,3,4), List(1)))
    println("should be true : " + List.hasSubsequence(List(1,2,3,4), List(1, 2)))
    println("should be true : " + List.hasSubsequence(List(1,2,3,4), List(2, 3)))
    println("should be true : " + List.hasSubsequence(List(1,2,3,4), List(4)))
    println("should be false : " + List.hasSubsequence(List(1,2,3,4), List(1, 3)))
    println("should be false : " + List.hasSubsequence(List(1,2,3,4), List(1,2,3,4,5)))*/

    val simpleTree = Branch( Branch(Leaf(1), Branch(Leaf(4), Leaf(100))), Leaf(3))
    println("size should be 7 : " + Tree.size(simpleTree))
    println("size should be 7 : " + Tree.sizeViaFold(simpleTree))

    println("size should be 100 : " + Tree.maximum(simpleTree))
    println("size should be 100 : " + Tree.maximumViaFold(simpleTree))

    println("size should be 3 : " + Tree.depth(simpleTree))
    println("size should be 3 : " + Tree.depthViaFold(simpleTree))


  }

}
