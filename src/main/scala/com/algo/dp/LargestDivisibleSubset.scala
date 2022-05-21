package com.algo.dp

object LargestDivisibleSubset extends App {

  val a = Array(1, 4, 16, 8, 7)
  val res = Solution.largestDivisibleSubset(a)
  println(res)

  object Solution {
    def largestDivisibleSubset(nums: Array[Int]): List[Int] = {
      lazy val sortedNums = nums.sorted
      lazy val dp = Array.fill[Int](nums.length)(1)
      lazy val prevElementIndex = Array.fill[Int](nums.length)(-1)

      @scala.annotation.tailrec
      def loopDp(num: Int, numIndex: Int, index: Int, max: Int): Int = {
        if (index < 0) {
          max
        } else {
          if (num % sortedNums(index) == 0) {
            val newMax =
              if (1 + dp(index) > max) {
                prevElementIndex(numIndex) = index
                1 + dp(index)
              } else {
                max
              }
            loopDp(num, numIndex, index - 1, newMax)
          } else {
            loopDp(num, numIndex, index - 1, max)
          }
        }
      }

      @scala.annotation.tailrec
      def find(index: Int): Unit = {
        if (index == sortedNums.length) {
          ()
        } else {
          dp(index) = loopDp(sortedNums(index), index, index - 1, 1)
          find(index + 1)
        }
      }

      @scala.annotation.tailrec
      def prepOutput(index: Int, output: Seq[Int]): Seq[Int] = {
        if(index == -1) {
          output
        } else {
          prepOutput(prevElementIndex(index), sortedNums(index) +: output)
        }
      }

      if(nums.length <= 1) {
        nums.toList
      } else {
        find(1)
        println(dp.mkString(","))
        println(prevElementIndex.mkString(","))
        val (_, outputStartingIndex) = dp.zipWithIndex.maxBy(_._1)
        prepOutput(outputStartingIndex, Nil).toList
      }
    }
  }

}
