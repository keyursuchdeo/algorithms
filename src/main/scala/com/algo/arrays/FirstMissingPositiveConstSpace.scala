package com.algo.arrays

object FirstMissingPositiveConstSpace extends App {

  val array = Array(-1, -2)
  val a = Solution.firstMissingPositive(array)
  println(a)

  object Solution {
    def firstMissingPositive(nums: Array[Int]): Int = {

      @scala.annotation.tailrec
      def fillNumsIndex(index: Int): Unit = {
        if(index == nums.length) {
          Unit
        } else {
          if(nums(index) > nums.length || nums(index) <= 0) {
            fillNumsIndex(index + 1)
          } else {
            swapAndFill(nums(index))
            fillNumsIndex(index + 1)
          }
        }
      }

      @scala.annotation.tailrec
      def swapAndFill(numIndex: Int): Unit = {
        if (numIndex > nums.length || numIndex <= 0) {
          Unit
        } else {
          val temp = nums(numIndex - 1)
          nums(numIndex - 1) = -1
          swapAndFill(temp)
        }
      }

      @scala.annotation.tailrec
      def replaceNegWith0(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else {
          if(nums(index) < 0) {
            nums(index) = 0
            replaceNegWith0(index + 1)
          } else {
            replaceNegWith0(index + 1)
          }
        }
      }

      replaceNegWith0(0)
      fillNumsIndex(0)
      println(nums.mkString(","))
      nums.zipWithIndex.find(_._1 >= 0).map(_._2).getOrElse(nums.length) + 1
    }
  }
}
