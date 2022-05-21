package com.algo.dp

object MaxSubArray extends App {
//  val a = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)
  val a = Array(1)
  val res = Solution.maxSubArray(a)
  println(res)

  object Solution {
    def maxSubArray(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def max(index: Int, maxSum: Int, currSum: Int): Int = {
        if (index == nums.length) {
          maxSum
        } else {
          val newCurrSum =
            if (currSum + nums(index) >= nums(index)) {
              currSum + nums(index)
            } else {
              nums(index)
            }
          if (newCurrSum >= maxSum) {
            max(index + 1, newCurrSum, newCurrSum)
          } else {
            max(index + 1, maxSum, newCurrSum)
          }
        }
      }

      max(1, nums(0), nums(0))
    }
  }

}
