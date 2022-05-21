package com.algo.dp

object JumpGameMinCount extends App {
  val a = Array(2, 3, 1, 1, 4)
//  val a = Array(3,2,1,0,4)
//  val a = Array(2, 5, 0, 0)
//  val a = Array(2, 0, 0)
  val res = Solution.jump(a)
  println(res)

  object Solution {
    def jump(nums: Array[Int]): Int = {
      val array = Array.fill(nums.length)(-1)
      @scala.annotation.tailrec
      def jump(currIndex: Int, lastReachableIndex: Int): Unit = {
        if(currIndex < 0) {
          ()
        } else {
          if ((currIndex + nums(currIndex)) - lastReachableIndex >= 0) {
            array(currIndex) = minJump(currIndex) + 1
            jump(currIndex - 1, currIndex)
          } else {
            jump(currIndex - 1, lastReachableIndex)
          }
        }
      }

      def minJump(currIndex: Int): Int = {
        @scala.annotation.tailrec
        def find(jumpLen: Int, min: Int): Int = {
          if(jumpLen == nums(currIndex) + 1) {
            min
          } else {
            if(currIndex + jumpLen < nums.length && array(currIndex + jumpLen) >= 0) {
              if (array(currIndex + jumpLen) < min) {
                find(jumpLen + 1, array(currIndex + jumpLen))
              } else {
                find(jumpLen + 1, min)
              }
            } else {
              find(jumpLen + 1, min)
            }
          }
        }

        find(1, Int.MaxValue)
      }

      array(nums.length - 1) = 0
      jump(nums.length - 2, nums.length - 1)
      println(array.mkString(","))
      array.head
    }
  }
}
