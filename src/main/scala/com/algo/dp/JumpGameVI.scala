package com.algo.dp

object JumpGameVI extends App {
  object Solution {
    def maxResult(nums: Array[Int], k: Int): Int = {
      val maxScores = Array.fill[Option[Int]](nums.length)(None)
      def calculate(index: Int): Option[Int] = {
        if(index >= nums.length) {
          None
        } else {
          maxScores(index) match {
            case Some(value) =>
              maxScores(index)
            case None =>
              val value =
                if (index + 1 == nums.length) {
                  nums(index)
                } else {
                  nums(index) + (index + 1 to index + k).flatMap(calculate).max
                }
              maxScores(index) = Option(value)
              maxScores(index)
          }
        }
      }

      calculate(0).getOrElse(0)
    }
  }
}
