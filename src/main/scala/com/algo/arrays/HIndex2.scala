package com.algo.arrays

object HIndex2 extends App {
  object Solution {
    def hIndex(citations: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(low: Int, high: Int): Int = {
        if(high < low) {
          citations.length - low
        } else {
          val mid = (low + high) / 2
          if(citations(mid) == citations.length - mid) {
            citations(mid)
          } else if (citations(mid) < citations.length - mid) {
            find(mid + 1, high)
          } else {
            find(low, mid - 1)
          }
        }
      }

      find(0, citations.length - 1)
    }
  }
}
