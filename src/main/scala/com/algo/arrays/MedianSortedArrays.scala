package com.algo.arrays

object MedianSortedArrays extends App {

  val nums1 = Array(1, 3, 8, 9, 15)
  val nums2 = Array(7, 11, 18, 19, 21, 25)

//  val nums1 = Array(1, 2)
//  val nums2 = Array(3, 4)

  val a = Solution.findMedianSortedArrays(nums1, nums2)
  println(a)

  object Solution {
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
      val totalLen = nums1.length + nums2.length
      val medianElementIndex = (totalLen + 1) / 2
      val leftArrayLen = medianElementIndex + 1
      val (sArray, lArray) = if(nums1.length < nums2.length) (nums1, nums2) else (nums2, nums1)
      val minElemsFromLargeArray = leftArrayLen - sArray.length
      val maxElemsFromLargeArray = if (leftArrayLen < lArray.length) leftArrayLen else lArray.length

      @scala.annotation.tailrec
      def medianElemLargeArray(l: Int, h: Int, lIndex: Int, sIndex: Int): (Int, Int) = {
        if (l > h) {
          (lIndex, sIndex)
        } else {
          val lArrayLastContriElemIndex = Math.ceil((l + h) / 2.0).toInt
          val lArrayContriElemsCount = lArrayLastContriElemIndex + 1
          val sArrayContriElemsCount = leftArrayLen - lArrayContriElemsCount
          val sArrayLastContriElemIndex = sArrayContriElemsCount - 1
          if(lArray(lArrayLastContriElemIndex) < sArray(sArrayLastContriElemIndex + 1)) {
            if(sArray(sArrayLastContriElemIndex) < lArray(lArrayLastContriElemIndex + 1)) {
              (lArrayLastContriElemIndex, sArrayLastContriElemIndex)
            } else {
              medianElemLargeArray(lArrayLastContriElemIndex + 1, h, lIndex, sIndex)
            }
          } else {
            medianElemLargeArray(l, lArrayLastContriElemIndex - 1, lIndex, sIndex)
          }
        }
      }

      val (lArrayIndex, sArrayIndex) =
        medianElemLargeArray(minElemsFromLargeArray, maxElemsFromLargeArray, -1, -1)
      val (mValue1, mValue2) =
        if(lArray(lArrayIndex) < sArray(sArrayIndex)) {
          (lArray(lArrayIndex), sArray(sArrayIndex))
        } else {
          (sArray(sArrayIndex), lArray(lArrayIndex))
        }
      if (totalLen % 2 == 1) {
        mValue1
      } else {
        (mValue1 + mValue2) / 2.0
      }
    }
  }
}
