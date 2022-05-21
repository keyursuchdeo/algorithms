package com.algo.arrays

object MaxNumberOfKSumPairs extends App {
  object Solution {
    def maxOperations(nums: Array[Int], k: Int): Int = {
      @scala.annotation.tailrec
      def countOperations(index: Int, count: Int, map: Map[Int, Int] = Map()): Int = {
        if(index == nums.length) {
          count
        } else {
          map.get(k - nums(index)) match {
            case Some(freq) if freq > 1 =>
              countOperations(index + 1, count + 1, map + ((k - nums(index)) -> (freq - 1)))
            case Some(freq) if freq == 1 =>
              countOperations(index + 1, count + 1, map - (k - nums(index)))
            case _ =>
              countOperations(index + 1, count, map + (nums(index) -> (map.getOrElse(nums(index), 0) + 1)))
          }
        }
      }

      countOperations(0, 0)
    }
  }
}
