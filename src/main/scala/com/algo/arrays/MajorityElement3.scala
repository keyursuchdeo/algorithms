package com.algo.arrays

import scala.collection.mutable

object MajorityElement3 extends App {

  val a = Array(2,2,1,1,1,2,2)
  val res = Solution.majorityElement(a)
  println(res)

  object Solution {
    def majorityElement(nums: Array[Int]): Int = {
      if (nums.length == 1) {
        nums.head
      } else {
        find(nums)
      }
    }

    private def find(nums: Array[Int]): Int = {
      var map = mutable.Map[Int, Int]()
      nums.foreach(num => {
        map = map + (num -> (map.getOrElse(num, 0) + 1))
      })
      map.maxBy(_._2)._1
    }
  }

}
