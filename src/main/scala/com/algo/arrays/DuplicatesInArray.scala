package com.algo.arrays

object DuplicatesInArray extends App {

//  val a = Array(4,3,2,7,8,2,3,1)
  val a = Array(3, 2, 2, 1, 1)
  val res = Solution.findDuplicates(a)
  println(res)

  object Solution {
    def findDuplicates(nums: Array[Int]): List[Int] = {
      @scala.annotation.tailrec
      def find(index: Int): Unit = {
//        println(nums.mkString(","))
        if(index == nums.length) {
          ()
        } else if (nums(index) < 0) {
          find(index + 1)
        } else {
          if(nums(index) - 1 == index) {
            nums(index) = -1
            find(index + 1)
          } else {
            val temp = nums(index)
            nums(index) = 0
            loop(temp)
            find(index + 1)
          }
        }
      }

      @scala.annotation.tailrec
      def loop(num: Int): Unit = {
        val temp = nums(num - 1)
        if(temp <= 0) {
          nums(num - 1) = temp - 1
        } else {
          nums(num - 1) = -1
          loop(temp)
        }
      }

      find(0)
      nums.zipWithIndex.filter(_._1 == -2).map(_._2 + 1).toList
    }
  }

}
