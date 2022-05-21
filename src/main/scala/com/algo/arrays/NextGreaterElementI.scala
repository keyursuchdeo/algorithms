package com.algo.arrays

object NextGreaterElementI extends App {
  object Solution {
    def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
      def findNextGreaterThan(num: Int): Int = {
        @scala.annotation.tailrec
        def findNextGreater(index: Int, numFound: Boolean): Int = {
          if(index == nums2.length) {
            -1
          } else {
            if(nums2(index) > num && numFound) {
              nums2(index)
            } else if (nums2(index) == num) {
              findNextGreater(index + 1, numFound = true)
            } else {
              findNextGreater(index + 1, numFound)
            }
           }
        }
        findNextGreater(0, numFound = false)
      }
      @scala.annotation.tailrec
      def find(index: Int = 0, output: Seq[Int] = Nil): Seq[Int] = {
        if(index == nums1.length) {
          output.reverse
        } else {
          val num = findNextGreaterThan(nums1(index))
          find(index + 1, num +: output)
        }
      }

      find().toArray
    }
  }
}
