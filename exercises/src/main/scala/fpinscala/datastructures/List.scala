package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h,Nil)
      case Cons(_, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case  Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)( ( _ , res) => res + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)((x, y) => x + y)
  }

  def productFoldLeft(l: List[Double]): Double = {
    foldLeft(l, 1.0)(_ * _)
  }

  def lenghtFoldLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((y, x) => y + 1)
  }



  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def appendFoldRight[A](l: List[A], l2: List[A]): List[A] = {
    foldRight(l, l2)(Cons(_,_))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())((x,y) => append(x,y))
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((h,t) => Cons(h+1, t))
  }

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((h,t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((h, t) => Cons(f(h), t))
  }

  def map_without_stack[A, B](l: List[A])(f: A => B): List[B] = {
    val buff = new collection.mutable.ListBuffer[B]
    @annotation.tailrec
    def loop(l: List[A]): Unit =
      l match {
        case Nil => List[B]()
        case Cons(h,t) => buff += f(h); loop(t)
      }
    loop(l)
    List(buff.toList: _*) // convert the buff list to our type of list
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val buff = new collection.mutable.ListBuffer[A]
    def loop(as: List[A]): Unit =
      as match {
        case Nil => List[A]()
        case Cons(h, t) => if(!f(h)) buff += h; loop(t)
      }
    loop(as)
    List(buff.toList: _*) // convert the buff list to our type of list
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val buff = new collection.mutable.ListBuffer[List[B]]
    @annotation.tailrec
    def loop(l: List[A]): Unit =
      l match {
        case Nil => List[B]()
        case Cons(h,t) => buff += f(h); loop(t)
      }
    loop(as)

    concat(List(buff.toList: _*)) // convert the buff list to our type of list, and concat the resulting list
  }

  def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) Nil else List(a) )
  }

  def addingElements(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_,Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah+bh, addingElements(at,bt))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (_,Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah,bh), zipWith(at,bt)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =  {

    @annotation.tailrec
    def loop[A](sup: List[A], sub: List[A], matching: Boolean): Boolean =  {
      (sup, sub, matching) match {
        case (_, Nil, true) => true
        case (_, Nil, false) => false
        case (Nil, _, _) => false
        case (Cons(h1, t1), Cons(h2, t2), true) =>  if (h1 == h2) loop(t1, t2, true) else false
        case (Cons(h1, t1), Cons(h2, t2), false) => if (h1 == h2) loop(t1, t2, true) else loop(t1, sub, false)
      }
    }

    (sup, sub) match  {
      case (_,Nil) => false
      case (Nil, _) => false
      case (_, _) => if (length(sup) >= length(sub)) loop(sup, sub, false) else false
    }
  }


}
