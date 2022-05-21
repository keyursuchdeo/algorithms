package com.algo.stack

object NextGreaterElementI2 extends App {
  object Solution {
    def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
      @scala.annotation.tailrec
      def findNextGreater(index: Int, elements: Seq[Int], nextGreater: Map[Int, Int]): Map[Int, Int] = {
        if(index == nums2.length) {
          nextGreater
        } else {
          if(elements.isEmpty || nums2(index) < elements.head) {
            findNextGreater(index + 1, nums2(index) +: elements, nextGreater)
          } else {
            findNextGreater(index, elements.tail, nextGreater + (elements.head -> nums2(index)))
          }
        }
      }

      @scala.annotation.tailrec
      def prepOutput(index: Int, nextGreater: Map[Int, Int], output: Array[Int]): Array[Int] = {
        if(index == nums1.length) {
          output
        } else {
          output(index) = nextGreater.getOrElse(nums1(index), -1)
          prepOutput(index + 1, nextGreater, output)
        }
      }

      val map = findNextGreater(0, Nil, Map())
      prepOutput(0, map, new Array[Int](nums1.length))
    }
  }
}
