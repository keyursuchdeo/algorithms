package com.algo.dp

object MaximumPerformanceTeam extends App {
  object Solution {
    def maxPerformance(n: Int, speed: Array[Int], efficiency: Array[Int], k: Int): Int = {
      val performances: Array[Array[Int]] = Array.fill[Int](n, k + 1)(-1)
      def calculate(index: Int, remainingK: Int, totalSpeed: Int, minEfficiency: Int): Int = {
        if(remainingK == 0) {
          totalSpeed * minEfficiency
        } else if (index == n) {
          Int.MinValue
        } else {
          if(performances(index)(remainingK) != -1) {
            performances(index)(remainingK)
          } else {
           val performance =
             Math.max(
               calculate(index + 1, remainingK, totalSpeed, minEfficiency),
               calculate(index + 1, remainingK - 1, totalSpeed + speed(index) , Math.min(minEfficiency, efficiency(index)))
             )
            performances(index)(remainingK) = performance
            performance
          }
        }
      }

      val output = calculate(0, k, 0, Int.MaxValue)
      println(performances.map(_.mkString(",")).mkString("|"))
      output
    }
  }
}
