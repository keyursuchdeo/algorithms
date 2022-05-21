package com.algo.arrays

object MergeSortedArray extends App {
  object Solution {
    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {

      def moveNums1ElementsUpTo(index: Int): Unit = {
        @scala.annotation.tailrec
        def move(currIndex: Int): Unit = {
          if(currIndex == index - 1) {
            ()
          } else {
            nums1(currIndex + 1) = nums1(currIndex)
            move(currIndex - 1)
          }
        }
        move(m - 1)
      }

      @scala.annotation.tailrec
      def performMerge(num2Index: Int, num1Index: Int, num1Count: Int): Unit = {
        if(num2Index == n) {
          ()
        } else if(num1Count == m) {
          Array.copy(nums2, num2Index, nums1, num1Index, n - num2Index)
        } else {
          if(nums1(num1Count) <= nums2(num2Index)) {
            performMerge(num2Index, num1Index + 1, num1Count + 1)
          } else {
            moveNums1ElementsUpTo(num1Index)
            nums1(num1Index) = nums2(num2Index)
            performMerge(num2Index + 1, num1Index + 1, num1Count)
          }
        }
      }

      performMerge(0, 0, 0)
    }
  }
}
