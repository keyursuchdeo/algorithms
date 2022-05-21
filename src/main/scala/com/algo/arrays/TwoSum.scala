package com.algo.arrays

object TwoSum extends App {
  val a = Array(2, 1, 8, 11, 15)
  val t = 9
  val res = Solution.twoSum(a, t)
  println(res.mkString(","))
  object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      @scala.annotation.tailrec
      def find(index: Int, remainingSum: Int, indices: Seq[Int], startingIndex: Int): Seq[Int] = {
        if (index == nums.length) {
          find(startingIndex + 1, target, Nil, startingIndex + 1)
        } else {
          if(indices.isEmpty) {
            find(index + 1, remainingSum - nums(index), index +: indices, startingIndex)
          } else {
            if (nums(index) == remainingSum) {
              index +: indices
            } else {
              find(index + 1, remainingSum, indices, startingIndex)
            }
          }
        }
      }
      find(0, target, Nil, 0).toArray
    }
  }
}
