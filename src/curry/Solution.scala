package curry
object Solution {
  // Curry: coverts a function f of two arguments into a function of one argument that partially applies f
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a:A) => (b:B) => f(a,b)

  // Reverse Curry
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a:A, b:B) => f(a)(b)

  // Implemement the higher order function that composes two functions
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))
}