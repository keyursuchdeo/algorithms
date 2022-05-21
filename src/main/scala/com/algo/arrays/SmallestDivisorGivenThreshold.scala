package com.algo.arrays

object SmallestDivisorGivenThreshold extends App {

//  val n = Array(1, 2, 5, 9)
//  val t = 6

  val n = Array(2,3,5,7,14)
  val t = 11

  val res = Solution.smallestDivisor(n, t)
  println(res)

  object Solution {
    def smallestDivisor(nums: Array[Int], threshold: Int): Int = {
      def calculateResult(divisor: Double): Int = {
        nums.map(num => Math.ceil(num / divisor)).sum.toInt
      }

      @scala.annotation.tailrec
      def findSmallest(low: Int, high: Int): Int = {
        if(high <= low) {
          low
        } else {
          val mid = (high + low) / 2
          val result = calculateResult(mid)
          println(s"$low $high $mid $result")
          if(result <= threshold) {
            findSmallest(low, mid)
          } else {
            findSmallest(mid + 1, high)
          }
        }
      }

      findSmallest(1, threshold)
    }
  }
}
