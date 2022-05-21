package com.algo.arrays

object MajorityElement2 extends App {

  val a = Array(3, 2, 1)
  val res = Solution.majorityElement(a)
  println(res)

  object Solution {
    def majorityElement(nums: Array[Int]): List[Int] = {
      lazy val oneThirdCount: Double = nums.length / 3.0
      @scala.annotation.tailrec
      def possibleMajorityElem(index: Int, currMajElem: Int, currMajElemCount: Int, excludedElem: Option[Int]): Int = {
        if(index == nums.length) {
          currMajElem
        } else {
          excludedElem match {
            case None =>
              if(nums(index) == currMajElem) {
                possibleMajorityElem(index + 1, currMajElem, currMajElemCount + 1, excludedElem)
              } else if (currMajElemCount == 1) {
                possibleMajorityElem(index + 1, nums(index), currMajElemCount, excludedElem)
              } else {
                possibleMajorityElem(index + 1, currMajElem, currMajElemCount - 1, excludedElem)
              }
            case Some(ex) =>
              if(nums(index) == ex) {
                possibleMajorityElem(index + 1, currMajElem, currMajElemCount, excludedElem)
              } else {
                if(nums(index) == currMajElem) {
                  possibleMajorityElem(index + 1, currMajElem, currMajElemCount + 1, excludedElem)
                } else if (currMajElemCount == 1) {
                  possibleMajorityElem(index + 1, nums(index), currMajElemCount, excludedElem)
                } else {
                  possibleMajorityElem(index + 1, currMajElem, currMajElemCount - 1, excludedElem)
                }
              }
          }
        }
      }
      def majorityElem(possibleElem: Int) =
        if (nums.count(_ == possibleElem) > oneThirdCount) possibleElem else -1

      if(nums.nonEmpty) {
        val possElem1 = possibleMajorityElem(0, nums(0), 1, None)
        val m1 = majorityElem(possElem1)

        val possElem2 = possibleMajorityElem(0, nums(0), 1, Option(m1))
        val m2 = majorityElem(possElem2)

        List(m1, m2).filter(_ > -1).distinct
      } else Nil
    }
  }
}
