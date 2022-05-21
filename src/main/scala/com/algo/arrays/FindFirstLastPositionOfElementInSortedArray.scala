package com.algo.arrays

object FindFirstLastPositionOfElementInSortedArray extends App {
  object Solution {
    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
      var first = Int.MaxValue
      var last = -1
      def search(low: Int, high: Int): Int = {
        if(high < low) {
          -1
        } else {
          val mid = (low + high) / 2
          if(nums(mid) == target) {
            first = Math.min(mid, first)
            last = Math.max(mid, last)
            search(low, mid - 1)
            search(mid + 1, high)
          } else if (nums(mid) < target) {
            search(mid + 1, high)
          } else {
            search(low, mid - 1)
          }
        }
      }

      search(0, nums.length - 1)
      if (first == Int.MaxValue) -1 else first
      Array(first, last)
    }
  }
}
