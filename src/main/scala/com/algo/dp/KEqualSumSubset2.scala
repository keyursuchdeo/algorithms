package com.algo.dp

import scala.collection.mutable

object KEqualSumSubset2 extends App {
    val a = Array(4, 3, 2, 3, 5, 2, 1)
    val k = 4
//  val a = Array(10, 10, 10, 7, 7, 7, 7, 7, 7, 6, 6, 6)
//  val k = 3
//    val a = Array(9,10,1,7,2,7,1,1,1,3)
//    val k = 3
  val res = Solution.canPartitionKSubsets(a, k)
  println(res)

  object Solution {
    def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean = {
      if (k == 1) true else if (nums.length < k) false else canPartition(nums, k)
    }

    private def canPartition(nums: Array[Int], k: Int): Boolean = {
      val total = nums.sum
      val partitionSum = total / k

      @scala.annotation.tailrec
      def fillBucket(index: Int, currSum: Int, remainingSum: Int, bucketIndices: Seq[Int], allBucketIndices: Seq[Seq[Int]], stack: mutable.Stack[(Int, Int, Int, Seq[Int])]): Seq[Seq[Int]] = {
        if (currSum == partitionSum) {
          if (stack.isEmpty) {
            bucketIndices +: allBucketIndices
          } else {
            val (poppedIndex, poppedCurrSum, poppedRemainingSum, poppedBucketIndices) = stack.pop()
            fillBucket(
              poppedIndex, poppedCurrSum, poppedRemainingSum, poppedBucketIndices, bucketIndices +: allBucketIndices, stack
            )
          }
        } else if (remainingSum < partitionSum - currSum) {
          if (stack.isEmpty) {
            allBucketIndices
          } else {
            val (poppedIndex, poppedCurrSum, poppedRemainingSum, poppedBucketIndices) = stack.pop()
            fillBucket(
              poppedIndex, poppedCurrSum, poppedRemainingSum, poppedBucketIndices, allBucketIndices, stack
            )
          }
        } else if (currSum + nums(index) <= partitionSum) {
          stack.push((index + 1, currSum, remainingSum - nums(index), bucketIndices))
          fillBucket(
            index + 1, currSum + nums(index), remainingSum - nums(index), index +: bucketIndices, allBucketIndices, stack)
        } else {
          fillBucket(
            index + 1, currSum, remainingSum - nums(index), bucketIndices, allBucketIndices, stack)
        }
      }

      val allBucketIndices = fillBucket(0, 0, total, Nil, Nil, mutable.Stack())
//      println(allBucketIndices)
      val numOfDist = allBucketIndices.length

      @scala.annotation.tailrec
      def findUniqueDist(index: Int, nextIndex: Int, uniqueCount: Int, consumedIndices: Seq[Int]): Int = {
        if (uniqueCount == k || index == numOfDist) {
          uniqueCount
        } else if (nextIndex == numOfDist) {
          findUniqueDist(index + 1, index + 2, uniqueCount, consumedIndices)
        } else {
          if (allBucketIndices(index).diff(allBucketIndices(nextIndex)) == allBucketIndices(index) &&
            allBucketIndices(nextIndex).diff(consumedIndices) == allBucketIndices(nextIndex)) {
            val (unconsumedIndices1, count1) =
              if (allBucketIndices(index).diff(consumedIndices) == allBucketIndices(index)) {
                (allBucketIndices(index), 1)
              } else {
                (Nil, 0)
              }
            val (unconsumedIndices2, count2) =
              if (allBucketIndices(nextIndex).diff(consumedIndices) == allBucketIndices(nextIndex)) {
                (allBucketIndices(nextIndex), 1)
              } else {
                (Nil, 0)
              }
//            println(unconsumedIndices1)
//            println(unconsumedIndices2)
            findUniqueDist(nextIndex, nextIndex + 1, uniqueCount + count1 + count2,
              consumedIndices ++ unconsumedIndices1 ++ unconsumedIndices2)
          } else {
            findUniqueDist(index, nextIndex + 1, uniqueCount, consumedIndices)
          }
        }
      }

      val count = findUniqueDist(0, 1, 0, Nil)
      count == k
    }
  }

}
