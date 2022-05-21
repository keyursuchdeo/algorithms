package com.algo.dp

object JumpGame2 extends App {
//  val a = Array(2, 3, 1, 1, 4)
//  val a = Array(3,2,1,0,4)
//  val a = Array(2, 5, 0, 0)
  val a = Array(2, 0, 0)
  val res = Solution.canJump(a)
  println(res)

  object Solution {
    def canJump(nums: Array[Int]): Boolean = {
      val array = Array.fill(nums.length)(false)
      @scala.annotation.tailrec
      def jump(currIndex: Int, lastReachableIndex: Int): Unit = {
        if(currIndex < 0) {
          ()
        } else {
          if ((currIndex + nums(currIndex)) - lastReachableIndex >= 0) {
            array(currIndex) = true
            jump(currIndex - 1, currIndex)
          } else {
            array(currIndex) = false
            jump(currIndex - 1, lastReachableIndex)
          }
        }
      }

      array(nums.length - 1) = true
      jump(nums.length - 2, nums.length - 1)
      println(array.mkString(","))
      array.head
    }
  }
}
