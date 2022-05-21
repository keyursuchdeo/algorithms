package com.algo.arrays

object ValidParenthesis extends App {
  object Solution {
    def isValid(s: String): Boolean = {

      @scala.annotation.tailrec
      def check(index: Int, stack: Seq[Char]): Boolean = {
        if(index == s.length) {
          stack.isEmpty
        } else {
          s(index) match {
            case '(' | '{' | '[' =>
              check(index + 1, s(index) +: stack)
            case ')' if stack.headOption == Option('(') =>
              check(index + 1, stack.tail)
            case '}' if stack.headOption == Option('{') =>
              check(index + 1, stack.tail)
            case ']' if stack.headOption == Option('[') =>
              check(index + 1, stack.tail)
            case _ => false
          }
        }
      }
      check(0, Nil)
    }
  }
}
