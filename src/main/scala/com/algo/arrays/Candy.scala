package com.algo.arrays

object Candy extends App {
  object Solution {
    def candy(ratings: Array[Int]): Int = {
      val candyDistribution = new Array[Int](ratings.length)

      @scala.annotation.tailrec
      def findIndexWithMinRatings(index: Int, minRatingIndex: Int): Int = {
        if(index == ratings.length) {
          minRatingIndex
        } else {
          if(ratings(index) < ratings(minRatingIndex)) {
            findIndexWithMinRatings(index + 1, index)
          } else {
            findIndexWithMinRatings(index + 1, minRatingIndex)
          }
        }
      }

      @scala.annotation.tailrec
      def distributeFromUpward(index: Int ): Unit = {
        if(index == ratings.length) {
          ()
        } else {
          if(ratings(index) == ratings(index - 1)) {
            if (ratings(index) <= ratings(index + 1)) {
              candyDistribution(index) = 1
            } else {
              candyDistribution(index) = candyDistribution(index - 1) + 1
            }
          } else if (ratings(index) < ratings(index - 1)) {
            candyDistribution(index) = Math.max(1, candyDistribution(index - 1) - 1)
          } else {
            candyDistribution(index) = candyDistribution(index - 1) + 1
          }
          distributeFromUpward(index + 1)
        }
      }

      @scala.annotation.tailrec
      def distributeFromDownward(index: Int ): Unit = {
        if(index < 0) {
          ()
        } else {
          if(ratings(index) == ratings(index + 1)) {
            candyDistribution(index) = 1
          } else if (ratings(index) < ratings(index + 1)) {
            candyDistribution(index) = Math.max(1, candyDistribution(index + 1) - 1)
          } else {
            candyDistribution(index) = candyDistribution(index + 1) + 1
          }
          distributeFromDownward(index - 1)
        }
      }

      val minIndex = findIndexWithMinRatings(1, 0)
      candyDistribution(minIndex) = 1
      distributeFromUpward(minIndex + 1)
      distributeFromDownward(minIndex - 1)
      println(candyDistribution.mkString(","))
      candyDistribution.sum
    }
  }
}
