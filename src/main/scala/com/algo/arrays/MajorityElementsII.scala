package com.algo.arrays

object MajorityElementsII extends App {

  object Solution {
    def majorityElement(nums: Array[Int]): List[Int] = {
      @scala.annotation.tailrec
      def findMajority(index: Int, optNum1: Option[Int] = None, count1: Int = 0, optNum2: Option[Int] = None, count2: Int = 0): (Option[Int], Option[Int]) = {
        if (index == nums.length) {
          (optNum1, optNum2)
        } else {
          (optNum1, optNum2) match {
            case (Some(num1), _) if nums(index) == num1 =>
              findMajority(index + 1, optNum1, count1 + 1, optNum2, count2)
            case (_, Some(num2)) if nums(index) == num2 =>
              findMajority(index + 1, optNum1, count1, optNum2, count2 + 1)
            case (None, _) =>
              findMajority(index + 1, Option(nums(index)), count1 + 1, optNum2, count2)
            case (_, None) =>
              findMajority(index + 1, optNum1, count1, Option(nums(index)), count2 + 1)
            case (Some(num1), Some(num2)) if nums(index) != num1 && nums(index) != num2 && count1 == 0 =>
              findMajority(index + 1, Option(nums(index)), count1 + 1, optNum2, count2)
            case (Some(num1), Some(num2)) if nums(index) != num1 && nums(index) != num2 && count2 == 0 =>
              findMajority(index + 1, optNum1, count1, Option(nums(index)), count2 + 1)
            case (Some(num1), Some(num2)) if nums(index) != num1 && nums(index) != num2 =>
              findMajority(index + 1, optNum1, count1 - 1, optNum2, count2 - 1)
          }
        }
      }

      def count(num: Int): Int = nums.count(_ == num)

      val ((num1, num1Count), (num2, num2Count)) =

        findMajority(0) match {
          case (Some(num1), Some(num2)) =>
            ((num1, count(num1)), (num2, count(num2)))
          case (Some(num1), None) =>
            ((num1, count(num1)), (-1, 0))
          case (None, Some(num2)) =>
            ((-1, 0), (num2, count(num2)))
          case (None, None) =>
            ((-1, 0), (-1, 0))
        }
      val size = Math.floor(nums.length / 3)
      if(num1Count > size && num2Count > size) {
        List(num1, num2)
      } else if (num1Count > size) {
        List(num1)
      } else if (num2Count > size) {
        List(num2)
      } else {
        Nil
      }
    }
  }

}
