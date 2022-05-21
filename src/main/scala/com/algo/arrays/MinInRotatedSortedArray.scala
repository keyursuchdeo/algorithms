package com.algo.arrays

object MinInRotatedSortedArray extends App {

  val n = Array(3, 4, 5, 1, 2)
  val res = Solution.findMin(n)
  println(res)

  object Solution {
    def findMin(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(low: Int, high: Int): Int = {
        val mid = (low + high) / 2
        if(nums(mid) >= nums(low) && nums(mid) < nums(high)) {
          nums(low)
        } else if (nums(mid) > nums(low)) {
          find(mid + 1, high)
        } else {
          find(low, mid)
        }

      }
      find(0, nums.length - 1)
    }
  }
}
