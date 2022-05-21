package com.algo.arrays

object MaximumPointsFromCards extends App {
  object Solution {
    def maxScore(cardPoints: Array[Int], k: Int): Int = {
      def sumTill(index: Int): Int = {
        @scala.annotation.tailrec
        def calculate(currIndex: Int, sum: Int): Int = {
          if(currIndex > index) {
            sum
          } else {
            calculate(currIndex + 1, sum + cardPoints(currIndex))
          }
        }
        calculate(0, 0)
      }

      @scala.annotation.tailrec
      def calculateMax(low: Int, high: Int, total: Int, max: Int): Int = {
        if(low < 0) {
          max
        } else {
          val updatedTotal = total - cardPoints(low) + cardPoints(high)
          calculateMax(low - 1, high - 1, updatedTotal, Math.max(max, updatedTotal))
        }
      }

      val sumTillK = sumTill(k - 1)
      calculateMax(k - 1, cardPoints.length - 1, sumTillK, sumTillK)

    }
  }
}
