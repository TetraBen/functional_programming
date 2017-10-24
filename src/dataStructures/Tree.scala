package dataStructures

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)((l,r) => 1 + l + r)

  def maximum(t: Tree[Int]) : Int = fold(t)(x => x)((l,r) => l max r)

  def depth[A](t: Tree[A]) : Int = fold(t)(_ => 0)((l,r) => (l+1) max (r+1))

  def map[A,B](t: Tree[A])(f:A => B) : Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(new Branch(_,_))

  def fold[A,B](t: Tree[A])(lf: A => B)(bf: (B,B) => B) : B = t match {
    case Branch(l,r) => bf( fold(l)(lf)(bf), fold(r)(lf)(bf))
    case Leaf(x) => lf(x)
  }
}