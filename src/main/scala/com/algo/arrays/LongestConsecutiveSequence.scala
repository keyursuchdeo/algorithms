package com.algo.arrays

object LongestConsecutiveSequence extends App {

  val a = Array(100,4,200,1,3,2)
  val res = Solution.longestConsecutive(a)
  println(res)

  object Solution {
    def longestConsecutive(nums: Array[Int]): Int = {
      val allNums = nums.toSet
      var visitedNums = Set[Int]()

      @scala.annotation.tailrec
      def calculate(index: Int, longest: Int): Int = {
        if(index == nums.length) {
          longest
        } else {
          val currLongest = dfsFrom(nums(index))
          calculate(index + 1, Math.max(longest, currLongest))
        }
      }

      def dfsFrom(num: Int): Int = {
        if(visitedNums.contains(num)) {
          0
        } else {
          visitedNums = visitedNums + num
          if(allNums.contains(num + 1) && allNums.contains(num - 1)) {
            1 + dfsFrom(num + 1) + dfsFrom(num - 1)
          } else if (allNums.contains(num + 1)) {
            1 + dfsFrom(num + 1)
          } else if (allNums.contains(num - 1)) {
            1 + dfsFrom(num - 1)
          } else {
            1
          }
        }
      }

      calculate(0, 0)
    }
  }
}
