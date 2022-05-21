package com.algo.dp

object MaxNonOverlappingSubArraysTargetSum extends App {

  val n = Array(5, 1, 5)
  val t = 6

  val res = Solution.maxNonOverlapping(n, t)
  println(res)

  object Solution {
    def maxNonOverlapping(nums: Array[Int], target: Int): Int = {

      var map: Map[Int, Int] = Map[Int, Int]()

      def count(index: Int, currSum: Int): Int = {
        if (index == nums.length) {
          0
        } else {
          map.get(index) match {
            case Some(value) => value
            case None =>
              val value =
                if (currSum + nums(index) == target) {
                  1 + count(index + 1, 0)
                } else {
                  Math.max(
                    count(index + 1, currSum + nums(index)),
                    count(index + 1, 0)
                  )
                }
              map = map + (index -> value)
              value
          }
        }
      }

      count(0, 0)
    }
  }

}
