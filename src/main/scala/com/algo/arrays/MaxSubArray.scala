package com.algo.arrays

object MaxSubArray extends App {
  object Solution {
    def maxSubArray(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(index: Int, maxSum: Int, prevIndexMaxSum: Int): Int = {
        if(index == nums.length) {
          maxSum
        } else {
          if(index == 0) {
            find(index + 1, nums(index), nums(index))
          } else {
            val currIndexMaxSum = Math.max(nums(index), nums(index) + prevIndexMaxSum)
            find(index + 1, Math.max(maxSum, currIndexMaxSum) , currIndexMaxSum)
          }
        }
      }

      find(0, 0, 0)
    }
  }
}
