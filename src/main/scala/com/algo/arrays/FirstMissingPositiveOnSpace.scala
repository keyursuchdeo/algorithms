package com.algo.arrays

object FirstMissingPositiveOnSpace extends App {

  val array = Array(7,8,9,11,12)
  val a = Solution.firstMissingPositive(array)
  println(a)

  object Solution {
    def firstMissingPositive(nums: Array[Int]): Int = {
      val numsIndex = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def fillNumsIndex(index: Int): Unit = {
        if(index == nums.length) {
          Unit
        } else {
          if (nums(index) > nums.length || nums(index) <= 0) {
            fillNumsIndex(index + 1)
          } else {
            numsIndex(nums(index) - 1) = -1
            fillNumsIndex(index + 1)
          }
        }

      }

      fillNumsIndex(0)
      println(numsIndex.mkString(","))
      numsIndex.zipWithIndex.find(_._1 == 0).map(_._2).getOrElse(nums.length) + 1
    }
  }
}
