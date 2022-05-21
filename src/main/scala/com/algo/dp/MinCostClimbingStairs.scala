package com.algo.dp

object MinCostClimbingStairs extends App {
  object Solution {
    def minCostClimbingStairs(cost: Array[Int]): Int = {
      val minCosts = Array.fill[Int](cost.length)(-1)

      def calculate(index: Int): Int = {
        if(index >= cost.length) {
          0
        } else {
          if(minCosts(index) != -1) {
            minCosts(index)
          } else {
            minCosts(index) =
              cost(index) + Math.min(
                calculate(index + 1),
                calculate(index + 2)
              )
            minCosts(index)
          }

        }
      }

      Math.min(calculate(0), calculate(1))
    }
  }
}
