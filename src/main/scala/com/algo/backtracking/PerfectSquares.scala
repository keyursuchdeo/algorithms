package com.algo.backtracking

object PerfectSquares extends App {
  val num = 12
  val res = Solution.numSquares(num)
  println(res)

  object Solution {
    def numSquares(n: Int): Int = {
      val dp = new Array[Int](n + 1)

      @scala.annotation.tailrec
      def find(index: Int): Unit = {
        if(index == n + 1) {
          ()
        } else if (index <= 1) {
          dp(index) = index
          find(index + 1)
        } else {
          val sqrt: Double = Math.sqrt(index)
          if(sqrt == sqrt.toInt) {
            dp(index) = 1
          } else {
            dp(index) = findMin(sqrt.toInt, index)
//            dp(index) = 1 + dp(index - Math.pow(sqrt.toInt, 2).toInt)
          }
          find(index + 1)
        }
      }

      def findMin(nearestSquare: Int, index: Int): Int = {
        @scala.annotation.tailrec
        def find(currNearestSquare: Int, minCount: Int): Int = {
          if(currNearestSquare < 1) {
            minCount
          } else {
            val currMinCount = 1 + dp(index - Math.pow(currNearestSquare, 2).toInt)
            find(currNearestSquare - 1, Math.min(minCount, currMinCount))
          }
        }
        find(nearestSquare, Int.MaxValue)
      }

      find(0)
      println(dp.mkString(","))
      dp(n)
    }
  }
}
