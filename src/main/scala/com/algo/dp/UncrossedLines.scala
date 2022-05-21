package com.algo.dp

object UncrossedLines extends App {

  val a = Array(1,3,7,1,7,5)
  val b = Array(1,9,2,5,1)
  val res = Solution.maxUncrossedLines(a, b)
  println(res)

  object Solution {
    def maxUncrossedLines(A: Array[Int], B: Array[Int]): Int = {
      val array: Array[Array[Int]] = Array.fill(A.length, B.length)(-1)
      def max(aIndex: Int, bIndex: Int): Int = {
        if(aIndex == A.length || bIndex == B.length) {
          0
        } else if (array(aIndex)(bIndex) != -1) {
          array(aIndex)(bIndex)
        } else {
          val output =
          if (A(aIndex) == B(bIndex)) {
            1 + max(aIndex + 1, bIndex + 1)
          } else {
            Math.max(
              max(aIndex, bIndex + 1),
              max(aIndex + 1, bIndex)
            )
          }
          array(aIndex)(bIndex) = output
          output
        }
      }

      max(0, 0)
    }
  }
}
