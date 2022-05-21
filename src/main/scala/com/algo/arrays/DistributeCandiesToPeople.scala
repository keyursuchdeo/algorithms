package com.algo.arrays

object DistributeCandiesToPeople extends App {
  object Solution {
    def distributeCandies(candies: Int, num_people: Int): Array[Int] = {
      val candyDistribution = new Array[Int](num_people)

      def prevIndex(currIndex: Int) = {
        if(currIndex == 0) num_people - 1 else currIndex - 1
      }

      def nextIndex(currIndex: Int) = {
        if(currIndex == num_people - 1) 0 else currIndex + 1
      }

      @scala.annotation.tailrec
      def distribute(remainingCandies: Int, index: Int): Unit = {
        if(remainingCandies == 0) {
          ()
        } else {
          val prev = prevIndex(index)
          if(remainingCandies > candyDistribution(prev) + 1) {
            candyDistribution(index) = candyDistribution(index) + candyDistribution(prev) + 1
            distribute(remainingCandies - (candyDistribution(prev) + 1), nextIndex(index))
          } else {
            candyDistribution(index) = candyDistribution(index) + remainingCandies
          }

        }
      }
      distribute(candies, 0)
      candyDistribution
    }
  }
}
