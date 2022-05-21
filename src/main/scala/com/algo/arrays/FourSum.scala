package com.algo.arrays

object FourSum extends App {

  object Solution {
    def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
      val sortedNums = nums.sorted

      def validIndex(index: Int): Boolean = {
        index >= 0 && index < nums.length
      }

      def isSame(index1: Int, index2: Int) = {
        validIndex(index1) && validIndex(index2) && sortedNums(index1) == sortedNums(index2)
      }

      def twoSum(fromIndex: Int, currTarget: Int): List[List[Int]] = {
        @scala.annotation.tailrec
        def find(low: Int, high: Int, output: Set[List[Int]]): List[List[Int]] = {
          if (low >= high) {
            output.toList
          } else {
            val value = sortedNums(low) + sortedNums(high)
            if (value == currTarget) {
              find(low + 1, high - 1, output + List(sortedNums(low), sortedNums(high)))
            } else if (value > currTarget) {
              find(low, high - 1, output)
            } else {
              find(low + 1, high, output)
            }
          }
        }
        if (fromIndex == sortedNums.length) {
          Nil
        } else {
          find(fromIndex, nums.length - 1, Set())
        }
      }

      @scala.annotation.tailrec
      def threeSum(fromIndex: Int, count: Int, currTarget: Int, output: List[List[Int]] = Nil): List[List[Int]] = {
        if (fromIndex == sortedNums.length) {
          output
        } else {
          if(count == 0 || !isSame(fromIndex - 1, fromIndex)) {
            val twoSumOutputs: List[List[Int]] = twoSum(fromIndex + 1, currTarget - sortedNums(fromIndex))
            threeSum(fromIndex + 1, count + 1, currTarget,
              twoSumOutputs.map(twoSumOutput => sortedNums(fromIndex) +: twoSumOutput) ++ output)
          } else {
            threeSum(fromIndex + 1, count + 1, currTarget, output)
          }
        }
      }

      @scala.annotation.tailrec
      def find(index: Int, currTarget: Int, output: List[List[Int]] = Nil): List[List[Int]] = {
        if (index == sortedNums.length) {
          output
        } else {
          if(index == 0 || !isSame(index - 1, index)) {
            val threeSumOutputs = threeSum(index + 1, 0, currTarget - sortedNums(index))
            find(index + 1, currTarget,
              threeSumOutputs.map(threeSumOutput => sortedNums(index) +: threeSumOutput) ++ output)
          } else {
            find(index + 1, currTarget, output)
          }
        }
      }

      find(0, target)
    }
  }

}
