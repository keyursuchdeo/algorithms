package com.algo.arrays

object ShortestUnsortedContinuousSubarray3 extends App {

  val input = Array(1,3,5,4,2)
  val res = Solution.findUnsortedSubarray(input)
  println(res)

  object Solution {
    def findUnsortedSubarray(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def findNonIncreasingIndex(index: Int = 1, minIndex: Int = -1): Int = {
        if(index == nums.length) {
          -1
        } else {
          if (nums(index) < nums(index - 1)) {
            if (minIndex == -1) {
              findNonIncreasingIndex(index + 1, index)
            } else {
              val newMinIndex = if(nums(minIndex) < nums(index)) minIndex else index
              findNonIncreasingIndex(index + 1, newMinIndex)
            }
          } else {
            findNonIncreasingIndex(index + 1, minIndex)
          }
        }
      }

      @scala.annotation.tailrec
      def findNonDecreasingIndex(index: Int = nums.length - 2): Int = {
        if(index < 0) {
          -1
        } else {
          if (nums(index) > nums(index + 1)) {
            index
          } else {
            findNonDecreasingIndex(index - 1)
          }
        }
      }

      def findMinPositionOf(index: Int): Int = {
        @scala.annotation.tailrec
        def findFrom(currIndex: Int): Int = {
          if(currIndex < 0) {
            0
          } else {
            if(nums(currIndex) <= nums(index)) {
              currIndex + 1
            } else {
              findFrom(currIndex - 1)
            }
          }
        }
        findFrom(index - 1)
      }

      def findMaxPositionOf(index: Int): Int = {
        @scala.annotation.tailrec
        def findFrom(currIndex: Int): Int = {
          if(currIndex == nums.length) {
            nums.length - 1
          } else {
            if(nums(currIndex) >= nums(index)) {
              currIndex - 1
            } else {
              findFrom(currIndex + 1)
            }
          }
        }
        findFrom(index + 1)
      }

      val lowIndex = findNonIncreasingIndex()
      if(lowIndex == -1) {
        0
      } else {
        val min = findMinPositionOf(lowIndex)
        val highIndex = findNonDecreasingIndex()
        if(highIndex == -1) {
          lowIndex - min + 1
        } else {
          val max = findMaxPositionOf(highIndex)
          max - min + 1
        }
      }
    }
  }
}
