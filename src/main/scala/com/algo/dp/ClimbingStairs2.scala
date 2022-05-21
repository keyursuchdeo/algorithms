package com.algo.dp

import scala.collection.mutable

object ClimbingStairs2 extends App {

  val res = Solution.climbStairs(5)
  println(res)

  object Solution {
    def climbStairs(n: Int): Int = {

      val numOfWaysToReach = new Array[Int](n)

      @scala.annotation.tailrec
      def climb(index: Int): Unit = {
        if(index == n) {
          ()
        } else {
          if(index == 0) {
            numOfWaysToReach(0) = 1
          } else if (index == 1) {
            numOfWaysToReach(1) = numOfWaysToReach(0) + 1
          } else {
            numOfWaysToReach(index) = numOfWaysToReach(index - 2) + numOfWaysToReach(index - 1)
          }
          climb(index + 1)
        }
      }

      climb(0)
      println(numOfWaysToReach.mkString(","))
      numOfWaysToReach(n - 1)
    }
  }
}
