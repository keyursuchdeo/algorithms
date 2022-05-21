package com.algo.arrays

object KthPositiveMissingIntegerBinSearch extends App {
  object Solution {
    def findKthPositive(arr: Array[Int], k: Int): Int = {

      @scala.annotation.tailrec
      def findFirstElementGreaterThanK(low: Int, high: Int): Int = {
        if(high <= low) {
          high
        } else {
          val mid = (low + high) / 2
          if(arr(mid) - mid - 1 >= k) {
            findFirstElementGreaterThanK(low, mid)
          } else {
            findFirstElementGreaterThanK(mid + 1, high)
          }
        }
      }

      findFirstElementGreaterThanK(0, arr.length) + k

    }
  }
}
