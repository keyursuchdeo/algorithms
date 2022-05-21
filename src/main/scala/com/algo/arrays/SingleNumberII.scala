package com.algo.arrays

object SingleNumberII extends App {

  val a = Array(-401451,-177656,-2147483646,-473874,-814645,-2147483646,-852036,-457533,-401451,-473874,-401451,-216555,-917279,-457533,-852036,-457533,-177656,-2147483646,-177656,-917279,-473874,-852036,-917279,-216555,-814645,2147483645,-2147483648,2147483645,-814645,2147483645,-216555)
  val res = Solution.singleNumber(a)
  println(res)

  object Solution {
    def singleNumber(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(index: Int, map: Map[Int, Int]): Int = {
        if(index == nums.length) {
          map.find(_._2 == 1).map(_._1).getOrElse(-1)
        } else {
          find(index + 1, map + (nums(index) -> (map.getOrElse(nums(index), 0) + 1)))
        }
      }
      find(0, Map())
    }
  }
}
