package com.algo.arrays

object MaxProduct3Nums extends App {
  object Solution {
    def maximumProduct(nums: Array[Int]): Int = {
      val sortedNums = nums.sorted
      val reversedSortedNums = sortedNums.reverse
      if(sortedNums.head > 0) {
        reversedSortedNums.take(3).product
      } else {
        val p1 = sortedNums.take(2).product * reversedSortedNums.head
        val p2 = reversedSortedNums.take(3).product
        Math.max(p1, p2)
      }
    }
  }
}
