package com.algo.dp

object MaxNonOverlappingSubArraysTargetSum5 extends App {

  val n = Array(5, 7, 24, -8, 3, 3, -8, 14, 1, 14, -10, 7, 15, -7, -5)
  val t = 50

  val res = Solution.maxNonOverlapping(n, t)
  println(res)

  object Solution {
    def maxNonOverlapping(nums: Array[Int], target: Int): Int = {

      var remainingSums = Map[Int, Int]()

      def sum(currIndex: Int): Int = {
        if (nums(currIndex) == target) {
          remainingSums = Map.empty
          1
        } else {
          remainingSums.get(target - nums(currIndex)) match {
            case Some(_) =>
              remainingSums = Map.empty
              1
            case None =>
              remainingSums =
                remainingSums.toSeq.map(sumFreq => {
                  val (sum, freq) = sumFreq
                  (sum + nums(currIndex), freq)
                }).toMap
              remainingSums = remainingSums + (nums(currIndex) -> (remainingSums.getOrElse(nums(currIndex), 0) + 1))
              0
          }
        }
      }

      @scala.annotation.tailrec
      def find(index: Int, curr: Int): Int = {
        if (index == nums.length) {
          curr
        } else {
          val output = sum(index)
          find(index + 1, curr + output)
        }
      }

      find(0, 0)
    }
  }

}
