package com.algo.arrays

object MajorityElement extends App {
  val a = Array(3, 2, 3)
  val res = Solution.majorityElement(a)
  println(res)
  object Solution {
    def majorityElement(nums: Array[Int]): Int = {
      def halfCount: Double = nums.length / 2.0
      @scala.annotation.tailrec
      def possibleMajorityElem(index: Int, currMajElem: Int, currMajElemCount: Int): Int = {
        if(index == nums.length) {
          currMajElem
        } else {
          if(nums(index) == currMajElem) {
            possibleMajorityElem(index + 1, currMajElem, currMajElemCount + 1)
          } else if (currMajElemCount == 1) {
            possibleMajorityElem(index + 1, nums(index), currMajElemCount)
          } else {
            possibleMajorityElem(index + 1, currMajElem, currMajElemCount - 1)
          }
        }
      }
      def majorityElem(possibleElem: Int) =
        if (nums.count(_ == possibleElem) > halfCount) possibleElem else -1

      val possElem = possibleMajorityElem(0, nums(0), 1)
      majorityElem(possElem)
    }
  }
}
