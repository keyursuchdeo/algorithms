package com.algo.arrays

object SubArraySum2 extends App {

  val a = Array(28,54,7,-70,22,65,-6)
  val k = 100
  val res = Solution.subarraySum(a, k)
  println(res)

  object Solution {
    def subarraySum(nums: Array[Int], k: Int): Int = {
      val sum = new Array[Int](nums.length + 1)

      @scala.annotation.tailrec
      def fillSum(index: Int): Unit = {
        if(index == nums.length + 1){
          ()
        } else {
          sum(index) = nums(index - 1) + sum(index - 1)
          fillSum(index + 1)
        }
      }

      @scala.annotation.tailrec
      def findAtStartIndex(startIndex: Int, count: Int): Int = {
        if(startIndex == nums.length) {
          count
        } else {
          val newCount = find(startIndex, startIndex + 1)
          findAtStartIndex(startIndex + 1, count + newCount)
        }
      }

      @scala.annotation.tailrec
      def find(startIndex: Int, index: Int, count: Int = 0): Int = {
        if (index == nums.length + 1) {
          count
        } else {
          if(sum(index) - sum(startIndex) == k) {
            find(startIndex, index + 1, count + 1)
          } else {
            find(startIndex, index + 1, count)
          }
        }
      }

      fillSum(1)
      println(sum.mkString(","))
      findAtStartIndex(0, 0)

    }
  }

}
