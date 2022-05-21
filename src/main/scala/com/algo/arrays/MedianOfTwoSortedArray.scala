package com.algo.arrays

import scala.util.Try

object MedianOfTwoSortedArray extends App {

  //  val a1 = Array(1, 3, 8, 9, 15)
  //  val a2 = Array(7, 11, 18, 19, 21, 25)

  //  val a2 = Array(23, 26, 31, 35)
  //  val a1 = Array(3, 5, 7, 9 , 11, 16)

//  val a1 = Array(1, 2, 3, 4, 5)
//  val a2 = Array(7, 8, 9, 10, 11, 12)

  val a1 = Array(-3)
  val a2 = Array(-2, -1)

  val res = Solution.findMedianSortedArrays(a1, a2)
  println(res)

  object Solution {
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

      val numOfElementsOnLeft = (nums1.length + nums2.length + 1) / 2

      def findMedian(array: Array[Int]): Double = {
        val mid = numOfElementsOnLeft - 1
        if(array.length % 2 == 0) {
          (array(mid) + array(mid + 1)) / 2.0
        } else {
          array(mid).toDouble
        }
      }

      def check(sLimit: Int, sArray: Array[Int], oArray: Array[Int]): Int = {
        val oLimit = numOfElementsOnLeft - (sLimit + 1) - 1
        if (sLimit < 0) {
          0
        } else if ((oLimit + 1 == oArray.length || sArray(sLimit) <= oArray(oLimit + 1)) &&
          (sLimit + 1 == sArray.length || oArray(oLimit) <= sArray(sLimit + 1))) {
          0
        } else if (sLimit + 1 == sArray.length || oArray(oLimit) <= sArray(sLimit + 1)) {
          -1
        } else {
          1
        }
      }

      @scala.annotation.tailrec
      def find(sLow: Int, sHigh: Int, sArray: Array[Int], oArray: Array[Int]): Int = {
        if (sHigh < sLow) {
          -1
        } else {
          val sMid = (sLow + sHigh) / 2
          val output = check(sMid, sArray, oArray)
          if (output == 0) {
            sMid
          } else if (output == -1) {
            find(sLow, sMid - 1, sArray, oArray)
          } else {
            find(sMid + 1, sHigh, sArray, oArray)
          }
        }
      }

      def calculateOutput(s: Int, sArray: Array[Int], oArray: Array[Int]) = {
        val o = numOfElementsOnLeft - (s + 1) - 1
        if ((sArray.length + oArray.length) % 2 == 0) {
          if (s == -1) {
            (oArray(o) + Math.min(Try(oArray(o + 1)).toOption.getOrElse(Int.MaxValue), sArray(s + 1))) / 2.0
          } else if (o == -1) {
            (sArray(s) + oArray(o + 1)) / 2.0
          } else if (s == sArray.length - 1) {
            (Math.max(sArray(s), oArray(o)) + oArray(o + 1)) / 2.0
          } else {
            (Math.max(sArray(s), oArray(o)) + Math.min(oArray(o + 1), sArray(s + 1))) / 2.0
          }
        } else {
          if (s == -1) {
            oArray(o).toDouble
          } else if (o == -1) {
            sArray(s).toDouble
          } else {
            Math.max(sArray(s), oArray(o)).toDouble
          }
        }
      }

      if(nums1.isEmpty) {
        findMedian(nums2)
      } else if (nums2.isEmpty) {
        findMedian(nums1)
      } else {
        if (nums1.length <= nums2.length) {
          val index = find(0, nums1.length - 1, nums1, nums2)
          calculateOutput(index, nums1, nums2)
        } else {
          val index = find(0, nums2.length - 1, nums2, nums1)
          calculateOutput(index, nums2, nums1)
        }
      }
    }
  }

}
