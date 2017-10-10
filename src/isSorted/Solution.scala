package isSorted

// Implement isSorted which checks a given Array[A] is Sorted
// according to a given comparison function

object Solution {
  def isSorted[A] (aa:Array[A], ordered: (A,A) => Boolean):Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean =
      if (i >= aa.length-1) true
      else if (!ordered(aa(i), aa(i + 1))) false
      else loop(i + 1)
    loop(0)
  }
}