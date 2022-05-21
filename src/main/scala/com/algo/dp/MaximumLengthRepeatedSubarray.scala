package com.algo.dp

object MaximumLengthRepeatedSubarray extends App {
  object Solution {
    def findLength(nums1: Array[Int], nums2: Array[Int]): Int = {

      val maxLengths = Array.fill[Int](nums1.length, nums2.length)(-1)

      def findMaxLen(index1: Int, index2: Int): Int = {
        if(index1 == nums1.length || index2 == nums2.length) {
          0
        } else {
          if (maxLengths(index1)(index2) != -1) {
            maxLengths(index1)(index2)
          } else {
            maxLengths(index1)(index2) =
              if(nums1(index1) == nums2(index2)) {
                1 + findMaxLen(index1 + 1, index2 + 1)
              } else {
                Math.max(
                  findMaxLen(index1 + 1, index2 + 1),
                  Math.max(findMaxLen(index1, index2 + 1),
                    findMaxLen(index1 + 1, index2))
                )
              }
            maxLengths(index1)(index2)
          }
        }
      }

      findMaxLen(0, 0)
    }
  }
}
