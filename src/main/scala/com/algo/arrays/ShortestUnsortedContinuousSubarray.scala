package com.algo.arrays

object ShortestUnsortedContinuousSubarray extends App {

  val input = Array(2, 3, 3, 2, 4)
  val res = Solution.findUnsortedSubarray(input)
  println(res)

  object Solution {
    def findUnsortedSubarray(nums: Array[Int]): Int = {

      def findSizeOfSubArray(lowIndex: Int, highIndex: Int): Int = {
        println(highIndex)
        println(lowIndex)
        if(lowIndex == -1 && highIndex == -1) {
          0
        } else if (highIndex == -1) {
          2
        } else {
          highIndex - lowIndex + 1
        }
      }

      @scala.annotation.tailrec
      def find(index: Int = 1, lowIndex: Int = -1, highIndex: Int = -1): Int = {
        if(index == nums.length) {
          findSizeOfSubArray(lowIndex, highIndex)
        } else {
          if(nums(index) < nums(index - 1)) {
            if(lowIndex == -1) {
              find(index + 1, index - 1, highIndex)
            } else {
              find(index + 1, lowIndex, index)
            }
          } else if (nums(index) == nums(index - 1) &&
            ((highIndex != -1 && highIndex == index - 1) || (lowIndex != -1 && lowIndex == index - 2))) {
            find(index + 1, lowIndex, index)
          } else {
            find(index + 1, lowIndex, highIndex)
          }
        }
      }

      find()
    }
  }
}
