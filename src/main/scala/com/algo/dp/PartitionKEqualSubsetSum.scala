package com.algo.dp

import scala.collection.mutable

object Solution {
  var stateMap: mutable.Map[(Int, Int), Boolean] = mutable.Map.empty
//  def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean = {
//    val totalSum = nums.sum
//    if(nums.length <= k - 1 || totalSum % k != 0) {
//      false
//    } else {
//      stateMap.clear()
//      val bucketSum = Array.ofDim[Int](k)
//      canPartition(totalSum, bucketSum, index = 0, bucketSumIndex = 0, nums, k)
//    }
//  }

//  private def canPartition(totalSum: Int, bucketSums: Array[Int], index: Int, bucketSumIndex: Int, nums: Array[Int], k: Int): Boolean = {
//    if(bucketSums(bucketSumIndex) == totalSum / k) {
//      canPartition(totalSum, bucketSums, index, bucketSumIndex + 1, nums, k)
//    }
//  }
}

//object PartitionKEqualSubsetSum extends App {
////  val nums = Array(4, 3, 2, 3, 5, 2, 1)
//  val nums = Array(2, 2, 2, 2, 3, 4, 5)
//  val output = Solution.canPartitionKSubsets(nums, 4)
//  println(output)
//
//}
