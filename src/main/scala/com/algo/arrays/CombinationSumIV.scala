package com.algo.arrays

object CombinationSumIV extends App {

  val a = Array(1, 2, 3)
  val t = 4
  val res = Solution.combinationSum4(a, t)
  println(res)

  object Solution {
    def combinationSum4(nums: Array[Int], target: Int): Int = {
      val counts = Array.fill[Int](target + 1)(-1)

      def findCombinations(remainingTarget: Int): Int = {
        if (remainingTarget == 0) {
          1
        } else if (remainingTarget < 0) {
          0
        } else {
          if (counts(remainingTarget) != -1) {
            counts(remainingTarget)
          } else {
            val count =
              nums.map(num => {
                findCombinations(remainingTarget - num)
              }).sum
            counts(remainingTarget) = count
            count
          }
        }
      }

      findCombinations(target)
    }
  }

}
