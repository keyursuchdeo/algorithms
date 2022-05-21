package com.algo.arrays

object SmallestRangeII extends App {
  object Solution {
    def smallestRangeII(A: Array[Int], K: Int): Int = {
      val sortedA = A.sorted

      @scala.annotation.tailrec
      def find(index: Int, smallest: Int): Int = {
        if(index == sortedA.length - 1) {
          smallest
        } else {
          val high = Math.max(sortedA(A.length - 1) - K, sortedA(index) + K)
          val low = Math.min(sortedA(0) + K, sortedA(index + 1) - K)
          find(index + 1, Math.min(smallest, high - low))
        }
      }

      find(0, sortedA(A.length - 1) - sortedA(0))

    }
  }
}
