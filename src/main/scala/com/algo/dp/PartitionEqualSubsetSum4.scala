package com.algo.dp

object PartitionEqualSubsetSum4 extends App {
  object Solution {
    def canPartition(nums: Array[Int]): Boolean = {
      val sum = nums.sum
      val array = Array.fill[Option[Boolean]](nums.length, (sum/2 + 1))(None)

      def check(index: Int, currSum: Int): Boolean = {
        if(index == nums.length || currSum > sum / 2) {
          false
        } else if (currSum == sum / 2) {
          true
        } else {
          array(index)(currSum) match {
            case Some(output) => output
            case None =>
              val output = check(index + 1, currSum + nums(index)) || check(index + 1, currSum)
              array(index)(currSum) = Option(output)
              output
          }
        }
      }

      if(sum % 2 == 1) false else check(0, 0)
    }
  }
}
