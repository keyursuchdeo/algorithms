package com.algo.dp

import scala.collection.mutable

object Solution1 {
  var stateMap: mutable.Map[(Int, Int), Boolean] = mutable.Map.empty
  def canPartition(nums: Array[Int]): Boolean = {
    val totalSum = nums.sum
    if(nums.length <= 1 || totalSum % 2 != 0) {
      false
    } else {
      stateMap.clear()
      canPartition(totalSum, 0, 0, nums)
    }
  }

  private def canPartition(totalSum: Int, partSum: Int, index: Int, nums: Array[Int]): Boolean = {
    stateMap.getOrElse((partSum, index), if (partSum > totalSum / 2) {
      false
    } else if (index == nums.length) {
      partSum == totalSum / 2
    } else {
      val can = canPartition(totalSum, partSum + nums(index), index + 1, nums) ||
        canPartition(totalSum, partSum, index + 1, nums)
      stateMap = stateMap + ((partSum, index) -> can)
      can
    })
  }
}

object PartitionEqualSubsetSum extends App {
//  val nums = Array(1, 5, 11, 5)
//  val nums = Array(1, 2, 3, 5)
    val nums = Array(1, 2, 5)
//  val nums = Array(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,100)
  val output = Solution1.canPartition(nums)
  println(output)
}
