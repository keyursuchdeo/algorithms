package com.algo.dp

import scala.collection.mutable

object LongestValidParentheses extends App {
//  val s = "()(())"
//  val s = "()(()"
//  val s = "()()"
  val s = "((())"
  val res = Solution.longestValidParentheses(s)
  println(res)

  object Solution {
    def longestValidParentheses(s: String): Int = {
      val stack: mutable.Stack[Int] = new mutable.Stack[Int]()
      val array: Array[Int] = new Array[Int](s.length)

      @scala.annotation.tailrec
      def calculate(index: Int): Unit = {
        if (index == s.length) {
          ()
        } else {
          if (s(index) == '(') {
            stack.push(index)
            array(index) == 0
            calculate(index + 1)
          } else if (s(index) == ')') {
            if(stack.nonEmpty) {
              val openPIndex = stack.pop()
              if(openPIndex - 1 >= 0) {
                array(index) = 2 + array(openPIndex - 1) + array(index - 1)
              } else {
                array(index) = 2 + array(index - 1)
              }
              calculate(index + 1)
            } else {
              //break
              array(index) == 0
              calculate(index + 1)
            }
          } else {
            ()
          }
        }
      }
      if (s.isEmpty) 0 else {
        calculate(0)
        array.max
      }
    }
  }
}
