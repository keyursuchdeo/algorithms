package com.algo.arrays

object MaxSumCircularSubArray extends App {
//    val a = Array(1,-2,3,-2)
//    val a = Array(5, -3, 5)
//  val a = Array(2, -2, 2, 7, 8, 0)
    val a = Array(3,- 1,2,-1)
//    val a = Array(3,-2,2,-3)
//    val a = Array(-2,-3,-1)
  val res = Solution.maxSubarraySumCircular(a)
  println(res)

  object Solution {
    def maxSubarraySumCircular(A: Array[Int]): Int = {
      val maxSums = new Array[Int](A.length)
      val minSums = new Array[Int](A.length)

      @scala.annotation.tailrec
      def max(index: Int): Unit = {
        if (index == A.length) {
          ()
        } else if (index == 0) {
          maxSums(index) = A(index)
          minSums(index) = A(index)
          max(index + 1)
        } else {
          maxSums(index) =
            Math.max(
              maxSums(index - 1) + A(index),
              A(index)
            )
          minSums(index) =
            Math.min(
              minSums(index - 1) + A(index),
              A(index)
            )
          max(index + 1)
        }
      }

      max(0)
      println(maxSums.mkString(","))
      println(minSums.mkString(","))
      val maxSum = maxSums.max
      if(maxSum > 0) {
        Math.max(maxSum, (A.sum - minSums.min))
      } else {
        maxSum
      }
    }
  }

}
