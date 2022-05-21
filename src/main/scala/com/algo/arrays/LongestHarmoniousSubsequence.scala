package com.algo.arrays

object LongestHarmoniousSubsequence extends App {
  object Solution {
    def findLHS(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def prepNumFreqMap(index: Int, map: Map[Int, Int]): Map[Int, Int] = {
        if(index < 0) {
          map
        } else {
          map.get(nums(index)) match {
            case Some(count) =>
              prepNumFreqMap(index - 1, map + (nums(index) -> (count + 1)))
            case None =>
              prepNumFreqMap(index - 1, map + (nums(index) -> 1))
          }
        }
      }

      @scala.annotation.tailrec
      def find(index: Int, map: Map[Int, Int], max: Int): Int = {
        if(index == nums.length) {
          max
        } else {
          val num = nums(index)
          map.get(num) match {
            case Some(count) =>
              val neighbours = Math.max(map.getOrElse(num + 1, 0), map.getOrElse(num - 1, 0))
              val harmoniousSeqLen = if(neighbours == 0) 0 else count + neighbours
              find(index + 1, map - num, Math.max(max, harmoniousSeqLen))
            case None =>
              find(index + 1, map - num, max)
          }
        }
      }

      val numFreq = prepNumFreqMap(nums.length - 1, Map())
      find(index = 0, numFreq, Int.MinValue)
    }
  }
}
