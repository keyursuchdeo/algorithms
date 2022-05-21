package com.algo.dp

import scala.collection.mutable

object PartitionEqualSubsetSum2 extends App {
  object Solution {
    def canPartition(nums: Array[Int]): Boolean = {
      val sum = nums.sum
      var map = mutable.Map[(Int, Int), Boolean]()

      def check(index: Int, currSum: Int): Boolean = {
        if(index == nums.length || currSum > sum / 2) {
          false
        } else if (currSum == sum / 2) {
          true
        } else {
          map.get((index, currSum)) match {
            case Some(output) => output
            case None =>
              val output = check(index + 1, currSum + nums(index)) || check(index + 1, currSum)
              map = map + ((index, currSum) -> output)
              output
          }

        }
      }

      if(sum % 2 == 1) false else check(0, 0)
    }
  }
}
