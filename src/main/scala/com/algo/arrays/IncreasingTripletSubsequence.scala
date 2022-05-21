package com.algo.arrays

object IncreasingTripletSubsequence extends App {

  val n = Array(1,1,-2,6)
  val res = Solution.increasingTriplet(n)
  println(res)

  object Solution {
    def increasingTriplet(nums: Array[Int]): Boolean = {
      def find(index: Int = 1, lowest: Int, optMiddle: Option[Int] = None): Boolean = {
        if(index == nums.length) {
          false
        } else {
          optMiddle match {
            case Some(middle) if nums(index) > middle =>
              true
            case Some(_) if nums(index) < lowest =>
              find(index + 1, lowest, optMiddle) || find(index + 1, nums(index), None)
            case _ if nums(index) <= lowest =>
              find(index + 1, nums(index), optMiddle)
            case _ =>
              find(index + 1, lowest, Some(nums(index)))
          }
        }
      }

      if(nums.isEmpty) false else find(lowest = nums(0))
    }
  }
}
