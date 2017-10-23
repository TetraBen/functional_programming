package dataStructures

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A] (t: Tree[A]) : Int = t match {
    case Branch(l,r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]) : Int = t match {
    case Branch(l,r) => maximum(l) max maximum(r)
    case Leaf(x) => x
  }

  def depth[A](t: Tree[A]) : Int = t match {
    case Branch(l,r) => depth(l)+1 max depth(r)+1
    case Leaf(x) => 0
  }

  def map[A,B](t: Tree[A])(f:A => B) : Tree[B] = t match {
    case Branch(l,r) => new Branch(map(l)(f), map(r)(f))
    case Leaf(x) => new Leaf(f(x))
  }
}