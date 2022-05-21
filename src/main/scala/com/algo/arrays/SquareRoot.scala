package com.algo.arrays

object SquareRoot extends App {

  val res = Solution.mySqrt(2147483647)
  println(res)

  object Solution {
    def mySqrt(x: Int): Int = {
      @scala.annotation.tailrec
      def find(low: Int, high: Int): Int = {
        if(high < low) {
          low - 1
        } else {
          val mid = (low + high) / 2
          val midL = mid.toLong
          val value = (midL * midL)
          if(value == x) {
            mid
          } else if (value > x) {
            find(low, mid - 1)
          } else {
            find(mid + 1, high)
          }
        }
      }

      if(x <= 1) {
        x
      } else {
        find(1, x / 2)
      }
    }
  }
}
