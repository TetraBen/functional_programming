package dataStructures

sealed trait List[+A] {
  def length : Int = List.foldLeft(this, 0)((_, acc) => 1 + acc)
}
case object Nil extends List[Nothing]
case class Cons[A](head:A, tail:List[A]) extends List[A]

object List {

  def foldRight[A, B](xs: List[A],z:B)(f: (A,B) => B) : B = xs match {
    case Nil => z
    case Cons(x, xy) => f(x, foldRight(xy,z)(f))
  }

  def foldLeftViaFoldRight[A,B](list: List[A], default: B)(func: (B,A) => B): B =
    foldRight(list, (param:B) => param)((head,tail) => (param) => tail(func(param,head)))(default)

  def foldRightViaFoldLeft[A, B](xs: List[A], z:B)(f: (A,B) => B) : B = foldLeft(reverse(xs), z)(f)
  def foldLeft[A,B](xs:List[A],z:B)(f: (A,B) => B) : B = {

    @annotation.tailrec
    def go(xs:List[A],acc:B)(f: (A,B) => B) : B = xs match {
      case Cons(x,xy) => go(xy,f(x,acc))(f)
      case Nil => acc
    }
    go(xs,z)(f)
  }

  // def sum(xs:List[Int]) : Int = foldRight(xs,0)(_ + _)
  def sum(xs:List[Int]) : Int = foldLeft(xs,0)(_ + _)

  // def product(xs:List[Double]) : Double = foldRight(xs, 1.0)(_*_)
  def product(xs:List[Double]) : Double = foldLeft(xs, 1.0)(_*_)

  //def length[A](xs:List[A]) : Int = foldRight(xs, 0)((_, acc) => 1 + acc)
  def length[A](xs: List[A]) : Int = foldLeft(xs, 0)((_, acc) => acc + 1)
  // Tail of Nil being Nil is probably wrong. Could return an option here,
  // but for now just return an empty list
  def tail[A](xs:List[A]) : List[A] = xs match {
    case Cons(_, xy) => xy
    case Nil => Nil
  }

  def reverse[A](xs:List[A]) : List[A] = foldLeft(xs, Nil:List[A])(
    (x, acc) => Cons(x, acc)
  )

  def setHead[A](x:A, xs:List[A]) : List[A] = xs match {
    case Cons(_, xy) => Cons(x, xy)
    case Nil => Cons(x, Nil)
  }

  def drop[A](xs: List[A], i:Int) : List[A] =
    if (i <= 0) xs else xs match {
      case Cons(_, xy) => drop(xy, i-1)
      case Nil => Nil
  }

  def dropWhile[A](xs: List[A], f: A => Boolean) : List[A] = xs match {
    case Cons(x, xy) if f(x) => dropWhile(xy, f)
    case _ => xs
  }

//  def append[A](a1: List[A], a2: List[A]) : List[A] = a1 match {
//    case Nil => a2
//    case Cons(h, t) => append(t, Cons(h, a2))
//  }

  def append[A](a1: List[A], a2: List[A] ) : List[A] = foldRight(a1, a2)((x, y) => Cons(x,y))

  def catLists[A](xxs: List[List[A]]) : List[A] = foldRight(xxs, Nil:List[A])(append)

  def map[A,B](xs: List[A])(f: A => B) : List[B] = foldRight(xs, Nil:List[B])((head, tail) => Cons(f(head),tail))

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = foldRight(as, Nil:List[B])((h,t) => append(f(h),t))

  def add1(xs:List[Int]) : List[Int] = map(xs)(_ + 1)

  def doubleToString(xs:List[Double]) : List[String] = map(xs)(_.toString)

  def filter[A](xs: List[A])(f: A => Boolean) : List[A] =

    foldRight(xs, Nil:List[A])((head, tail) =>
      if (f(head)) Cons(head, tail) else tail
    )

  def flatFilter[A](xs: List[A])(f: A => Boolean) : List[A] = flatMap(xs)((x) => if(f(x)) List(x) else Nil)

  def filterOdd(x:List[Int]) : List[Int] = filter(x)(_ % 2 == 0)

  def zipWith[A,B](as:List[A],xs: List[A])(f: (A,A) => B) : List[B] = (as,xs) match {
    case (Nil, Nil) => Nil
    case (Cons(a,at), Cons(b,bt) ) => Cons(f(a,b), zipWith(at,bt)(f))
    case _ => throw new Error("mismatched lists")
  }

  def hasSubsequence[A](sup: List[A], sub:List[A]): Boolean = {

    def isSubsequence[A](a: A, b:A) : Boolean = a match {
      case Cons(x, xy) => b match {
        case Cons(c, cd) => if (c == x) isSubsequence(xy, cd) else false
        case Nil => true
      }
      case Nil => b == Nil
    }

    sup match {
      case Cons(h, t) => if (isSubsequence(sup, sub)) true else hasSubsequence(t, sub)
      case Nil => sub == Nil
    }
  }

  def zipAdd(as: List[Int], ab: List[Int]) = zipWith(as, ab)((x,y) => x + y)

  def init[A](l: List[A]) : List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => Nil
  }

  def apply[A](xs: A*): List[A] = {
    if (xs.isEmpty) Nil else Cons(xs.head, apply(xs.tail: _*))
  }

}
