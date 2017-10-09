package fib
// Write a recursive function to get the nth Fibonacci number
// (http://mng.bz/C29s). The first two Fibonacci numbers ar 0 and 1.
// The nth number is always the sum of the previous two -- the sequence begins 0,1,1,2,3,5
// Your definition should use a local tail recursive function

object Solution {
  def fib (x:Int) : Int = {
    @annotation.tailrec
    def go (num:Int, cur:Int, prev:Int) :Int =
      if (num == 0) prev
      else go(num - 1, cur + prev, cur)
    go(x,1,0)
  }
}
