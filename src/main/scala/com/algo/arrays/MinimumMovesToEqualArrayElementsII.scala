package com.algo.arrays

object MinimumMovesToEqualArrayElementsII extends App {
  object Solution {
    def minMoves2(nums: Array[Int]): Int = {
      val sortedNums = nums.sorted
      val median = sortedNums(nums.length / 2)
      nums.map(num => Math.abs(num - median)).sum
    }
  }
}
