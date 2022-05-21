package com.algo.arrays

object ValidateStackSequences extends App {

  object Solution {
    def validateStackSequences(pushed: Array[Int], popped: Array[Int]): Boolean = {
      @scala.annotation.tailrec
      def validate(pushIndex: Int, popIndex: Int, stack: Seq[Int] = Nil): Boolean = {
        if (popIndex == popped.length && stack.isEmpty) {
          true
        } else {
          if (stack.isEmpty || stack.head != popped(popIndex)) {
            if (pushIndex == pushed.length) {
              false
            } else {
              validate(pushIndex + 1, popIndex, pushed(pushIndex) +: stack)
            }
          } else {
            validate(pushIndex, popIndex + 1, stack.tail)
          }
        }
      }

      validate(0, 0)
    }
  }

}
