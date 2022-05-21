package com.algo.dp

object NumberOfLongestIncreasingSequence2 extends App {

//  val n = Array(5, 3, 6, 7)
  val n = Array(1, 2, 5, 4, 7)
//  val n = Array(2, 2, 2, 2, 2)
//  val n = Array(5, 5, 6, 7)
  val res = Solution.findNumberOfLIS(n)
  println(res)

  object Solution {
    def findNumberOfLIS(nums: Array[Int]): Int = {
      val lengths = Array.fill[Int](nums.length)(1)
      val counts = Array.fill[Int](nums.length)(1)

      def fillLenAndCountAt(index: Int): Unit = {
        @scala.annotation.tailrec
        def fill(currIndex: Int): Unit = {
          if(currIndex == index) {
            ()
          } else {
            if(nums(index) > nums(currIndex)) {
              if(lengths(currIndex) + 1 > lengths(index)) {
                lengths(index) = lengths(currIndex) + 1
                counts(index) = counts(currIndex)
              } else if(lengths(index) == lengths(currIndex) + 1) {
                counts(index) = counts(index) + counts(currIndex)
              } else {
                lengths(index) = lengths(index)
              }
              fill(currIndex + 1)
            } else {
              fill(currIndex + 1)
            }
          }
        }

        fill(0)
      }

      @scala.annotation.tailrec
      def calculate(index: Int = 1): Unit = {
        if(index == nums.length) {
          ()
        } else {
          fillLenAndCountAt(index)
          calculate(index + 1)
        }
      }

      def countOccurrencesOf(length: Int) = {
        @scala.annotation.tailrec
        def count(index: Int, currCount: Int = 0): Int = {
          if(index == nums.length) {
            currCount
          } else {
            if(lengths(index) == length) {
              count(index + 1, currCount + counts(index))
            } else {
              count(index + 1, currCount)
            }
          }
        }
        count(0)
      }

      if (nums.isEmpty) 0 else {
        calculate()
        println(lengths.mkString(","))
        println(counts.mkString(","))
        val maxLen = lengths.max
        countOccurrencesOf(maxLen)
      }
    }
  }
}
