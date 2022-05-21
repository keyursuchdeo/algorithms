package com.algo.arrays

object NumberOfSubarraysWithBoundedMaximum extends App {

  val a = Array(2, 1, 4, 3)
  val l = 2
  val r = 4

  val res = Solution.numSubarrayBoundedMax(a, l, r)
  println(res)

  object Solution {
    def numSubarrayBoundedMax(nums: Array[Int], left: Int, right: Int): Int = {
      @scala.annotation.tailrec
      def countWithin(bound: Int, ans: Int, count: Int, index: Int): Int = {
        if(index == nums.length) {
          ans
        } else {
          if(nums(index) <= bound) {
            countWithin(bound, ans + count + 1, count + 1, index + 1)
          } else {
            countWithin(bound, ans, 0, index + 1)
          }
        }
      }

      countWithin(right, 0, 0, 0) - countWithin(left - 1, 0, 0, 0)
    }
  }
}
