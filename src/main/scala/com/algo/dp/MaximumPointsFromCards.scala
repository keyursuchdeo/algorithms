package com.algo.dp

object MaximumPointsFromCards extends App {
  object Solution {
    def maxScore(cardPoints: Array[Int], k: Int): Int = {
      val scores: Array[Array[Int]] = Array.ofDim[Int](cardPoints.length, cardPoints.length)
      def calculate(low: Int, high: Int, remainingK: Int): Int = {
        if (remainingK == 0 || high < low || low > high) {
          0
        } else {
          if (scores(low)(high) > 0) {
            scores(low)(high)
          } else {
            val value =
              Math.max(
                cardPoints(low) + calculate(low + 1, high, remainingK - 1),
                cardPoints(high) + calculate(low, high - 1, remainingK - 1)
              )
            scores(low)(high) = value
            value
          }
        }
      }

      calculate(0, cardPoints.length - 1, k)
    }
  }

}
