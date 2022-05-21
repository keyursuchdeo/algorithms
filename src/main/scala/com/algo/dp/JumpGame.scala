package com.algo.dp

import scala.collection.mutable

object JumpGame extends App {
//  val a = Array(2, 3, 1, 1, 4)
//  val a = Array(3,2,1,0,4)
  val a = Array(2, 5, 0, 0)
  val res = Solution.canJump(a)
  println(res)

  object Solution {
    def canJump(nums: Array[Int]): Boolean = {
      val array = Array.fill(nums.length)(-1)
      def jump(index: Int): Int = {
        if (index == nums.length - 1) {
          1
        } else if (array(index) != -1) {
          array(index)
        } else {
          (1 to nums(index)).find(jumpSize => {
            println(s"jump from index $index by $jumpSize")
            val a = jump(index + jumpSize)
            array(index) = a
            a >= 1
          }).getOrElse(0)
        }
      }

      jump(0) >= 1
    }
  }
}
