package com.algo.arrays

object MinimumSizeSubArraySum extends App {

  val n = Array(2,3,1,2,4,3)
  val res = Solution.minSubArrayLen(7, n)
  println(res)

  object Solution {
    def minSubArrayLen(s: Int, nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def findSmallestSubArraySummingTo(low: Int, high: Int, currSum: Int, minLength: Int): Int = {
        if(low == nums.length || high == nums.length) {
          minLength
        } else {
          if(currSum + nums(high) < s) {
            findSmallestSubArraySummingTo(low, high + 1, currSum + nums(high), minLength)
          } else {
            findSmallestSubArraySummingTo(low + 1, high, currSum - nums(low), Math.min(minLength, high - low + 1))
          }
        }
      }

      if(nums.length == 1) {
        if(s == nums.head) {
          1
        } else {
          0
        }
      }  else {
        val len = findSmallestSubArraySummingTo(0, 0, 0, Int.MaxValue)
        if(len == Int.MaxValue) 0 else (nums.length - len)
      }
    }
  }
}
