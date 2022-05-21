package com.algo.stack

object NextGreaterElementII extends App {

  val a = Array(1, 2, 1)
  val res = Solution.nextGreaterElements(a)
  println(res.mkString(","))

  object Solution {
    def nextGreaterElements(nums: Array[Int]): Array[Int] = {

      @scala.annotation.tailrec
      def findForward(index: Int, loopCount: Int, stack: Seq[Int], output: Array[Int]): Array[Int] = {
        if(index == nums.length && loopCount == 2) {
          output
        } else if (index == nums.length) {
          findForward(0, loopCount + 1, stack, output)
        } else {
          if(stack.isEmpty) {
            findForward(index + 1, loopCount, index +: stack, output)
          } else {
            if(nums(index) > nums(stack.head) && output(stack.head) == -1) {
              output(stack.head) = nums(index)
              findForward(index, loopCount, stack.tail, output)
            } else {
              if(loopCount == 0) {
                findForward(index + 1, loopCount, index +: stack, output)
              } else {
                findForward(index + 1, loopCount, stack, output)
              }
            }
          }
        }
      }

      findForward(0, 0, Nil, Array.fill[Int](nums.length)(-1))
    }
  }
}
