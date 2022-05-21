package com.algo.stack

object DailyTemperatures extends App {
  object Solution {
    def dailyTemperatures(T: Array[Int]): Array[Int] = {
      @scala.annotation.tailrec
      def find(index: Int, stack: Seq[Int], output: Array[Int]): Array[Int] = {
        if(index == T.length) {
          output
        } else {
          if(stack.isEmpty) {
            find(index + 1, index +: stack, output)
          } else {
            if(T(index) > T(stack.head)) {
              output(stack.head) = index - stack.head
              find(index, stack.tail, output)
            } else {
              find(index + 1, index +: stack, output)
            }
          }
        }
      }

      find(0, Nil, new Array[Int](T.length))
    }
  }
}
