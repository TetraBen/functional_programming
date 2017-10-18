package dataStructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head:A, tail:List[A]) extends List[A]

object List {

  def sum(xs:List[Int]) : Int = xs match {
      case Nil => 0
      case Cons(x, xy) => x + sum(xy)
    }

  def product(xs:List[Double]) : Double = xs match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(a, ab) => a * product(ab)
  }

  // Tail of Nil being Nil is probably wrong. Could return an option here,
  // but for now just return an empty list
  def tail[A](xs:List[A]) : List[A] = xs match {
    case Cons(_, xy) => xy
    case Nil => Nil
  }

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

  def append[A](a1: List[A], a2: List[A]) : List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => append(t, Cons(h, a2))
  }

  def init[A](l: List[A]) : List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => Nil
  }

  def apply[A](xs: A*): List[A] = {
    if (xs.isEmpty) Nil else Cons(xs.head, apply(xs.tail: _*))
  }

}
