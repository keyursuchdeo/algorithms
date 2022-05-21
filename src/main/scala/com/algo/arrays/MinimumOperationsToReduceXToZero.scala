package com.algo.arrays

object MinimumOperationsToReduceXToZero extends App   {
  object Solution {
    def minOperations(nums: Array[Int], x: Int): Int = {
      val sum = nums.sum
      @scala.annotation.tailrec
      def findLongestSubArraySummingTo(index: Int, map: Map[Int, Int], prevSum: Int, maxLength: Int): Int = {
        if(index == nums.length) {
          maxLength
        } else {
          val num = nums(index)
          map.get(sum - num) match {
            case Some(value) =>
              findLongestSubArraySummingTo(index + 1, map + ((prevSum + num) -> index), prevSum + num, Math.max(maxLength, index - value))
            case None =>
              findLongestSubArraySummingTo(index + 1, map + ((prevSum + num) -> index), prevSum + num, maxLength)
          }
        }
      }

      val len = findLongestSubArraySummingTo(0, Map(), 0, -1)
      if(len == -1) -1 else (nums.length - len)
    }
  }
}
