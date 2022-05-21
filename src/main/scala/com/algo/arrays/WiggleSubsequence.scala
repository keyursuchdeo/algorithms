package com.algo.arrays

object WiggleSubsequence extends App {
  object Solution {
    def wiggleMaxLength(nums: Array[Int]): Int = {

      @scala.annotation.tailrec
      def find(index: Int = 1, lengthTillNow: Int = 1, prevDiff: Int = 0, prevIndex: Int = 0): Int = {
        if(index == nums.length) {
          lengthTillNow
        } else {
          if(nums(index) > nums(prevIndex)) {
            if(index == 1 || prevDiff <= 0) {
              find(index + 1, lengthTillNow + 1, nums(index) - nums(prevIndex), index)
            } else {
              find(index + 1, lengthTillNow, prevDiff + (nums(index) - nums(prevIndex)), index)
            }
          } else if (nums(index) < nums(prevIndex)) {
            if(index == 1 || prevDiff >= 0) {
              find(index + 1, lengthTillNow + 1, nums(index) - nums(prevIndex), index)
            } else {
              find(index + 1, lengthTillNow, prevDiff + (nums(index) - nums(prevIndex)), index)
            }
          } else {
            find(index + 1, lengthTillNow, prevDiff, prevIndex)
          }
        }
      }

      find()
    }
  }
}
