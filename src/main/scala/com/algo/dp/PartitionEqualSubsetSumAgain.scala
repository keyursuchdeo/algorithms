package com.algo.dp

object PartitionEqualSubsetSumAgain extends App {
  object Solution {
    def canPartition(nums: Array[Int]): Boolean = {
      var map: Map[(Int, Int, Int), Int] = Map[(Int, Int, Int), Int]()
      def check(index: Int, sum1: Int, sum2: Int): Int = {
        if(index == nums.length) {
          if(sum1 == sum2) 1 else 0
        } else {
          map.get((index, sum1, sum2)) match {
            case Some(output) => output
            case None =>
              map.get((index, sum2, sum1)) match {
                case Some(output) => output
                case None =>
                  val output = Math.max(check(index + 1, sum1 + nums(index), sum2), check(index + 1, sum1, sum2 + nums(index)))
                  map = map + ((index, sum1, sum2) -> output)
                  output
              }
          }
        }
      }

      check(0, 0, 0) == 1
    }
  }
}
