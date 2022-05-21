package com.algo.arrays

object RemoveAllAdjacentDuplicatesInString extends App {
  object Solution {
    def removeDuplicates(s: String): String = {
      val chars = s.toCharArray

      @scala.annotation.tailrec
      def remove(index: Int, stack: Seq[Char]): String = {
        if(index == chars.length) {
          stack.reverse.mkString
        } else {
          if(stack.isEmpty || stack.head != chars(index)) {
            remove(index + 1, chars(index) +: stack)
          } else {
            remove(index + 1, stack.tail)
          }
        }
      }

      remove(0, Nil)
    }
  }
}
