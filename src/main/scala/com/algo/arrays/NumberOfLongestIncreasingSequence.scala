package com.algo.arrays

object NumberOfLongestIncreasingSequence extends App {

//  val n = Array(5, 3, 6, 7)
  val n = Array(2, 1, 3, 4, 7)
//  val n = Array(2, 2, 2, 2, 2)
//  val n = Array(5, 5, 6, 7)
  val res = Solution.findNumberOfLIS(n)
  println(res)

  object Solution {
    def findNumberOfLIS(nums: Array[Int]): Int = {
      val lengths = new Array[Int](nums.length)
      val counts = Array.fill[Int](nums.length)(1)

      def calculateLISLength(index: Int): Int = {
        @scala.annotation.tailrec
        def calculate(currIndex: Int, max: Int): Int = {
          if(currIndex + 2 < max || currIndex < 0) {
            max
          } else {
            if(nums(index) > nums(currIndex)) {
              counts(index) =  counts(currIndex)
              if(lengths(currIndex) + 1 > max) {
                calculate(currIndex - 1, lengths(currIndex) + 1)
              } else {
                calculate(currIndex - 1, max)
              }
            } else if (nums(index) == nums(currIndex)) {
              counts(index) = counts(index) + counts(currIndex)
              calculate(currIndex - 1, max)
            } else {
              calculate(currIndex - 1, max)
            }
          }
        }
        calculate(index - 1, 1)
      }

      @scala.annotation.tailrec
      def calculateLISLengths(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else {
          lengths(index) = calculateLISLength(index)
          calculateLISLengths(index + 1)
        }
      }

      calculateLISLengths(0)
      val maxLength = lengths.max
      println(lengths.mkString(","))
      println(counts.mkString(","))
      1
    }
  }
}
