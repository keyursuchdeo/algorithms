package com.algo.sort

object ColourSort extends App {
  val a = Array(2, 0)
  Solution.sortColors(a)
  println(a.mkString(","))

  object Solution {
    def sortColors(nums: Array[Int]): Unit = {
      @scala.annotation.tailrec
      def sort(index: Int, optZeroEndIndex: Option[Int], optOneEndIndex: Option[Int], optTwoEndIndex: Option[Int]): Unit = {
        if(index == nums.length) {
          None
        } else {
          (optZeroEndIndex, optOneEndIndex, optTwoEndIndex) match {
            case (_, None, None) =>
              if(nums(index) == 0) {
                sort(index + 1, Option(index), optOneEndIndex, optTwoEndIndex)
              } else if (nums(index) == 1) {
                sort(index + 1, optZeroEndIndex, Option(index), optTwoEndIndex)
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }
            case (None, None, Some(_)) =>
              if(nums(index) == 0) {
                nums(0) = nums(index)
                nums(index) = 2
                sort(index + 1, Option(0), optOneEndIndex, Option(index))
              } else if (nums(index) == 1) {
                nums(0) = nums(index)
                nums(index) = 2
                sort(index + 1, optZeroEndIndex, Option(0), Option(index))
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }
            case (None, Some(_), None) =>
              if(nums(index) == 0) {
                nums(0) = nums(index)
                nums(index) = 1
                sort(index + 1, Option(0), Option(index), optTwoEndIndex)
              } else if (nums(index) == 1) {
                sort(index + 1, optZeroEndIndex, Option(index), optTwoEndIndex)
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }
            case (None, Some(oneEndIndex), Some(_)) =>
              if(nums(index) == 0) {
                nums(0) = nums(index)
                nums(index) = 2
                nums(oneEndIndex + 1) = 1
                sort(index + 1, Option(0), Option(oneEndIndex + 1), Option(index))
              } else if (nums(index) == 1) {
                nums(0) = nums(index)
                nums(index) = 2
                nums(oneEndIndex + 1) = 1
                sort(index + 1, optZeroEndIndex, Option(oneEndIndex + 1), Option(index))
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }
            case (Some(zeroEndIndex), None, Some(_)) =>
              if(nums(index) == 0) {
                nums(index) = 2
                nums(zeroEndIndex + 1) = 0
                sort(index + 1, Option(zeroEndIndex + 1), optOneEndIndex, Option(index))
              } else if (nums(index) == 1) {
                nums(index) = 2
                nums(zeroEndIndex + 1) = 1
                sort(index + 1, optZeroEndIndex, Option(zeroEndIndex + 1), Option(index))
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }
            case (Some(zeroEndIndex), Some(_), None) =>
              if(nums(index) == 0) {
                nums(index) = 1
                nums(zeroEndIndex + 1) = 0
                sort(index + 1, Option(zeroEndIndex + 1), Option(index), optTwoEndIndex)
              } else if (nums(index) == 1) {
                sort(index + 1, optZeroEndIndex, Option(index), optTwoEndIndex)
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }
            case (Some(zeroEndIndex), Some(oneEndIndex), Some(_)) =>
              if(nums(index) == 0) {
                nums(index) = 2
                nums(zeroEndIndex + 1) = 0
                nums(oneEndIndex + 1) = 1
                sort(index + 1, Option(zeroEndIndex + 1), Option(oneEndIndex + 1), Option(index))
              } else if (nums(index) == 1) {
                nums(index) = 2
                nums(oneEndIndex + 1) = 1
                sort(index + 1, optZeroEndIndex, Option(oneEndIndex + 1), Option(index))
              } else {
                sort(index + 1, optZeroEndIndex, optOneEndIndex, Option(index))
              }


          }
        }
      }
      sort(0, None, None, None)
    }
  }
}
