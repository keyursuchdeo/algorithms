package com.algo.sort

object MaximumGap extends App {
  object Solution {
    def maximumGap(nums: Array[Int]): Int = {
      if (nums.length < 2) {
        0
      } else {
        val high = nums.max
        val low = nums.min
        val numOfBuckets = Math.max((high - low) / (nums.length - 1), 1)
        val buckets: Array[Seq[Int]] = Array.fill[Seq[Int]](numOfBuckets)(Nil)

        @scala.annotation.tailrec
        def bucketNums(index: Int): Unit = {
          if(index == nums.length) {
            ()
          } else {
            val bucketIndex = (nums(index) - low) / numOfBuckets
            buckets(bucketIndex) = nums(index) +: buckets(bucketIndex)
            bucketNums(index + 1)
          }
        }

        @scala.annotation.tailrec
        def findMaxGap(bucketIndex: Int, prevHigh: Int, ans: Int): Int = {
          if(bucketIndex == buckets.length) {
            ans
          } else {
            if(buckets(bucketIndex).isEmpty) {
              findMaxGap(bucketIndex + 1, prevHigh, ans)
            } else {
              val bucketHigh = buckets(bucketIndex).max
              val bucketLow = buckets(bucketIndex).min
              if (prevHigh == 0) {
                findMaxGap(bucketIndex + 1, bucketHigh, ans)
              } else {
                findMaxGap(bucketIndex + 1, bucketHigh, Math.max(ans, bucketLow - prevHigh))
              }
            }
          }
        }


        bucketNums(0)
        findMaxGap(0, 0, 0)
      }
    }
  }
}
