package com.algo.arrays

object Pattern132Sorting extends App {

  val input = Array(-4,0,-3)
  val res = Solution.find132pattern(input)
  println(res)

  object Solution {
    def find132pattern(nums: Array[Int]): Boolean = {
      @scala.annotation.tailrec
      def find(index: Int, secondNum: Int, stack: Seq[Int]): Boolean = {
        if(index < 0) {
          false
        } else {
          if(nums(index) < secondNum) {
            true
          } else {
            stack.headOption match {
              case Some(head) if nums(index) > head =>
                find(index, head, stack.tail)
              case _ =>
                find(index - 1, secondNum, nums(index) +: stack)
            }
          }
        }
      }

      find(nums.length - 1, Int.MinValue, Nil)
    }
  }
}
