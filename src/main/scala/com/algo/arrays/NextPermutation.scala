package com.algo.arrays

object NextPermutation extends App {
  object Solution {
    def nextPermutation(nums: Array[Int]): Unit = {
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

      @scala.annotation.tailrec
      def findNextPermutationFromIndex(index: Int): (Int, Int) = {
        if(index < 0) {
          (-1, -1)
        } else {
          val swapIndex = findIndexOfNextLargerNumFrom(index)
          if(swapIndex == -1) {
            findNextPermutationFromIndex(index - 1)
          } else {
            (index, swapIndex)
          }
        }
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

      def shiftArrayElementsStarting(index: Int, upToIndex: Int): Unit = {
        @scala.annotation.tailrec
        def shift(currIndex: Int, numAtUpToIndex: Int): Unit = {
          if(currIndex == index) {
            nums(currIndex + 1) = nums(currIndex)
            nums(currIndex) = numAtUpToIndex
          } else {
            nums(currIndex + 1) = nums(currIndex)
            shift(currIndex - 1, numAtUpToIndex)
          }
        }

        shift(upToIndex - 1, nums(upToIndex))
      }

      @scala.annotation.tailrec
      def swapArrayElementsUntilLargerIsFound(index: Int): Unit = {
        if(index == nums.length - 1) {
          ()
        } else {
          if(nums(index) == nums(index + 1)) {
            swapArrayElementsUntilLargerIsFound(index + 1)
          } else if (nums(index) > nums(index + 1)) {
            swap(index, index + 1)
            swapArrayElementsUntilLargerIsFound(index + 1)
          } else {
            ()
          }
        }
      }

      val (index1, index2) = findNextPermutationFromIndex(nums.length - 2)
      if(index1 == -1) {
        reverseArray(0, nums.length - 1)
      } else {
        shiftArrayElementsStarting(index1, index2)
        swapArrayElementsUntilLargerIsFound(index2)
      }
    }
  }
}
