package com.algo.arrays

object NextPermutation2 extends App {
  object Solution {
    def nextPermutation(nums: Array[Int]): Unit = {
      def findIndexUpToWhichArrayIsDecreasing(): Int = {
        @scala.annotation.tailrec
        def find(currIndex: Int): Int = {
          if(currIndex < 0) {
            0
          } else {
            if(nums(currIndex) < nums(currIndex + 1)) {
              currIndex + 1
            } else {
              find(currIndex - 1)
            }
          }
        }

        find(nums.length - 2)
      }

      def findIndexOfNextLargerNumFrom(index: Int): Int = {
        @scala.annotation.tailrec
        def find(currIndex: Int, currNextLargestIndex: Int): Int = {
          if(currIndex == nums.length) {
            currNextLargestIndex
          } else {
            if(currNextLargestIndex == -1) {
              if(nums(currIndex) > nums(index)) {
                find(currIndex + 1, currIndex)
              } else {
                find(currIndex + 1, currNextLargestIndex)
              }
            } else {
              if(nums(currIndex) > nums(index) && nums(currIndex) < nums(currNextLargestIndex)) {
                find(currIndex + 1, currIndex)
              } else {
                find(currIndex + 1, currNextLargestIndex)
              }
            }
          }
        }
        find(index + 1, currNextLargestIndex = -1)
      }

      def swap(sIndex1: Int, sIndex2: Int): Unit = {
        val temp = nums(sIndex1)
        nums(sIndex1) = nums(sIndex2)
        nums(sIndex2) = temp
      }

      @scala.annotation.tailrec
      def reverseArray(begin: Int, end: Int): Unit = {
        if(end <= begin) {
          ()
        } else {
          swap(begin, end)
          reverseArray(begin + 1, end - 1)
        }
      }

      val indexOfDecreasingArray = findIndexUpToWhichArrayIsDecreasing()
      if(indexOfDecreasingArray == 0) {
        reverseArray(0, nums.length - 1)
      } else {
        val swapIndex = findIndexOfNextLargerNumFrom(indexOfDecreasingArray)
        swap(indexOfDecreasingArray, swapIndex)
        reverseArray(indexOfDecreasingArray, nums.length - 1)
      }


    }
  }
}
