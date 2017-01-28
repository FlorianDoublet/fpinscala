package fpinscala.laziness


trait Stream[+A] {

  def toListRec : List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toListRec
  }

  def toList : List[A] = {
    @annotation.tailrec
    def loop(stream: Stream[A], acc: List[A]) : List[A] = stream match {
      case Cons(h, t) => loop(t(), h()::acc)
      case _ => acc
    }

    loop(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }


  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n -1))
    case Cons(h, _) if n == 1 => Stream.cons(h(),Stream())
    case _ => Stream()
  }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (Stream(), 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
      case _ => None
    }


  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 =>  t().drop(n -1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) =>  Some(h(), t())
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()),t()))
      case _ => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (f(h)) Stream.cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => Stream.cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = {
    !zipWith(s)((a,b) => a == b).exists(_ == false)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val const: Stream[A] = Stream.cons(a, const)
    const
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(),h2()), (t1(),t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())),(t1(),t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None),(t1(),Stream.empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())),(Stream.empty,t2()))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this){
      case Cons(h, t) => Some((Stream.cons(h(),t()), t()))
      case _ => None
    }

  def tailsSol: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tailsViaScanRight exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

  def tailsViaScanRight: Stream[Stream[A]] =
    scanRight(Stream.empty[A])((h,t) => Stream(h).append(t)).takeWhile(_ != Stream.empty)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs(): Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a+b))
    loop(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def fibsViaUnfold(): Stream[Int] =
    unfold((0,1)){case (a,b) => Some(a, (b, a+b))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x+1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, x))

  val onesViaFold: Stream[Int] = constantViaUnfold(1)

}