package com.algo.arrays

object SubArrayProductLessThanK extends App {
  val a = Array(10, 5, 2, 6)
  val res = Solution.numSubarrayProductLessThanK(a, 100)
  println(res)

  object Solution {
    def numSubarrayProductLessThanK(nums: Array[Int], k: Int): Int = {
      @scala.annotation.tailrec
      def count(begin: Int, end: Int, productWithinWindow: Int, currCount: Int): Int = {
        println(s"$begin, $end, $currCount")
        if(begin == end && end == nums.length - 1) {
          if (nums(begin) < k) {
            currCount + 1
          } else {
            currCount
          }
        } else if (begin == end) {
          if (nums(begin) < k) {
            count(begin, end + 1, nums(begin), currCount + 1)
          } else {
            count(begin + 1, begin + 1, productWithinWindow, currCount)
          }
        } else if (end == nums.length - 1) {
          if(nums(end) * productWithinWindow < k) {
            count(begin + 1, end, productWithinWindow / nums(begin), currCount + 1)
          } else {
            count(begin + 1, end, productWithinWindow / nums(begin), currCount)
          }
        } else {
          if(nums(end) * productWithinWindow < k) {
            count(begin, end + 1, nums(end) * productWithinWindow, currCount + (end - begin) + 1)
          } else {
            count(begin + 1, end, productWithinWindow / nums(begin), currCount)
          }
        }
      }

      count(0, 0, 1, 0)
    }
  }
}
