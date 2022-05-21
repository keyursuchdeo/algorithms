package com.algo.dp

import scala.collection.mutable

object ClimbingStairs extends App {

  val res = Solution.climbStairs(1)
  println(res)

  object Solution {
    def climbStairs(n: Int): Int = {

      @scala.annotation.tailrec
      def climb(stack: mutable.Stack[(Int, Int)], totalStepsClimbed: Int, numOfWaysToClimb: Int): Int = {
        if (totalStepsClimbed == n) {
          if (stack.isEmpty) {
            numOfWaysToClimb + 1
          } else {
            val (nextStep, stepsClimbed) = stack.pop()
            climb(stack, stepsClimbed + nextStep, numOfWaysToClimb + 1)
          }
        } else {
          if (n - totalStepsClimbed == 1) {
            climb(stack, totalStepsClimbed + 1, numOfWaysToClimb)
          } else {
            climb(stack.push((2, totalStepsClimbed)), totalStepsClimbed + 1, numOfWaysToClimb)
          }
        }
      }

      climb(mutable.Stack[(Int, Int)](), 0, 0)
    }
  }
}
