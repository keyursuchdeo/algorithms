package com.algo.arrays

object MergeSortedArray2 extends App {
  object Solution {
    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {

      @scala.annotation.tailrec
      def performMerge(mIndex: Int, nIndex: Int, mergeIndex: Int): Unit = {
        if(mIndex < 0 && nIndex < 0) {
          ()
        } else if (mIndex < 0) {
          nums1(mergeIndex) = nums2(nIndex)
          performMerge(mIndex, nIndex - 1, mergeIndex - 1)
        } else if (nIndex < 0) {
          nums1(mergeIndex) = nums1(mIndex)
          performMerge(mIndex - 1, nIndex, mergeIndex - 1)
        } else {
          if(nums1(mIndex) > nums2(nIndex)) {
            nums1(mergeIndex) = nums1(mIndex)
            performMerge(mIndex - 1, nIndex, mergeIndex - 1)
          } else {
            nums1(mergeIndex) = nums2(nIndex)
            performMerge(mIndex, nIndex - 1, mergeIndex - 1)
          }
        }
      }

      performMerge(m - 1, n - 1, m + n - 1)
    }
  }
}
