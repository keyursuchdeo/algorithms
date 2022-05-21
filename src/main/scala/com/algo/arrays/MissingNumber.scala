package com.algo.arrays

object MissingNumber extends App {

  val a = Array(9,6,4,2,3,8,7,5,1)
  val res = Solution.missingNumber(a)
  println(res)

  object Solution {
    def missingNumber(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else {
          if(nums(index) >= nums.length || nums(index) == -1) {
            find(index + 1)
          } else {
            markFound(nums(index))
            find(index + 1)
          }
        }
      }

      @scala.annotation.tailrec
      def markFound(num: Int): Unit = {
        if(num >= nums.length || num == -1) {
          ()
        } else {
          val temp = nums(num)
          nums(num) = -1
          markFound(temp)
        }
      }

      find(0)
      println(nums.mkString(","))
      nums.zipWithIndex.find(_._1 > -1) match {
        case Some((_, index)) => index
        case _ => -1
      }
    }
  }
}
