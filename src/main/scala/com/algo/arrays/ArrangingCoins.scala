package com.algo.arrays

object ArrangingCoins extends App {

  val res = Solution.arrangeCoins(1804289383)
  println(res)

  object Solution {
    def arrangeCoins(n: Int): Int = {
      @scala.annotation.tailrec
      def find(low: Int, high: Int): Int = {
        if(high < low) {
          high
        } else {
          val mid = (low + high) / 2
          val value = calculateValue(mid.toLong)
          println(s"mid -> $mid value -> $value")
          if(value == n) {
            mid
          } else if (value < n) {
            find(mid + 1, high)
          } else {
            find(low, mid - 1)
          }
        }
      }

      def calculateValue(x: Long): Long = {
        ((x * x) + x) / 2
      }

      find(1, Math.ceil(Math.sqrt(2 * n.toDouble)).toInt)
    }
  }
}
