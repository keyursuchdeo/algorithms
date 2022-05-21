package com.algo.dp

object KEqualSumSubset extends App {
//  val a = Array(4, 3, 2, 3, 5, 2, 1)
//  val k = 4
  val a = Array(10,10,10,7,7,7,7,7,7,6,6,6)
  val k = 3
  val res = Solution.canPartitionKSubsets(a, k)
  println(res)

  object Solution {
    def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean = {
      if (k == 1) true else if(nums.length < k) false else canPartition(nums, k)
    }

    private def canPartition(nums: Array[Int], k: Int): Boolean = {
      println(nums.sum)
      val elementSumOfEachPartition = nums.sum / k
      println(elementSumOfEachPartition)
      val kSumBuckets: Array[Int] = new Array[Int](k)

      @scala.annotation.tailrec
      def partition(index: Int): Boolean = {
        if (index == nums.length) {
          true
        } else {
          if(nums(index) > elementSumOfEachPartition) {
            false
          } else {
            val bucket = findKSumBucketIndex(kSumBuckets, nums(index), elementSumOfEachPartition)
            bucket match {
              case Some(bucket) =>
                kSumBuckets(bucket) += nums(index)
                partition(index + 1)
              case None =>
                println(index)
                false
            }
          }
        }
      }

      def findKSumBucketIndex(buckets: Array[Int], currElement: Int, partitionSum: Int) = {
        @scala.annotation.tailrec
        def find(index: Int): Option[Int] = {
          if (index == buckets.length) {
            None
          } else {
            if(buckets(index) + currElement <= partitionSum) {
              Option(index)
            } else {
              find(index + 1)
            }
          }
        }
        find(0)
      }

      val a = partition(0)
      println(kSumBuckets.mkString(","))
      a
    }
  }
}
