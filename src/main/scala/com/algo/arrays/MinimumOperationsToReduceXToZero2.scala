package com.algo.arrays

object MinimumOperationsToReduceXToZero2 extends App   {

  val n = Array(1, 1, 4, 2, 3)
  val res = Solution.minOperations(n, 5)
  println(res)

  object Solution {
    def minOperations(nums: Array[Int], x: Int): Int = {
      val targetSum = nums.sum - x
      @scala.annotation.tailrec
      def findLongestSubArraySummingTo(low: Int, high: Int, currSum: Int, maxLength: Int): Int = {
        if(low == nums.length || high == nums.length) {
          maxLength
        } else {
          if(currSum + nums(high) < targetSum) {
            findLongestSubArraySummingTo(low, high + 1, currSum + nums(high), maxLength)
          } else if (currSum + nums(high) > targetSum) {
            findLongestSubArraySummingTo(low + 1, high, currSum - nums(low), maxLength)
          } else {
            findLongestSubArraySummingTo(low + 1, high, currSum - nums(low), Math.max(maxLength, high - low + 1))
          }
        }
      }

      if(nums.length == 1) {
        if(x == nums.head) {
          1
        } else {
          -1
        }
      }  else {
        val len = findLongestSubArraySummingTo(0, 0, 0, -1)
        if(len == -1) -1 else (nums.length - len)
      }
    }
  }
}
