package com.algo.arrays

object ShortestUnsortedContinuousSubarray2 extends App {

  val input = Array(1,3,2,2,2)
  val res = Solution.findUnsortedSubarray(input)
  println(res)

  object Solution {
    def findUnsortedSubarray(nums: Array[Int]): Int = {

      def findRightPosOfNumAt(index: Int): Int = {
        @scala.annotation.tailrec
        def findPos(currIndex: Int): Int = {
          if(currIndex == 0) {
            0
          } else {
            if(nums(currIndex) <= nums(index)) {
              currIndex + 1
            } else {
              findPos(currIndex - 1)
            }
          }
        }

        findPos(index - 1)
      }

      def findSizeOfSubArray(lowIndex: Int, highIndex: Int): Int = {
        println(highIndex)
        println(lowIndex)
        if(lowIndex == -1 && highIndex == -1) {
          0
        } else if (highIndex == -1) {
          lowIndex - findRightPosOfNumAt(lowIndex)
        } else {
          highIndex - findRightPosOfNumAt(lowIndex) + 1
        }
      }

      @scala.annotation.tailrec
      def find(index: Int = 1, lowIndex: Int = -1, highIndex: Int = -1): Int = {
        if(index == nums.length) {
          findSizeOfSubArray(lowIndex, highIndex)
        } else {
          if(nums(index) < nums(index - 1)) {
            if(lowIndex == -1) {
              find(index + 1, index, highIndex)
            } else {
              find(index + 1, lowIndex, index)
            }
          } else if (nums(index) == nums(index - 1)) {
            if(lowIndex == index - 1) {
              find(index + 1, index, highIndex)
            } else if (highIndex == index - 1) {
              find(index + 1, lowIndex, index)
            } else {
              find(index + 1, lowIndex, highIndex)
            }
          } else {
            find(index + 1, lowIndex, highIndex)
          }
        }
      }

      find()
    }
  }
}
