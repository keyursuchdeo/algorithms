package com.algo.arrays

object ScoreOfParentheses extends App {
  object Solution {
    def scoreOfParentheses(S: String): Int = {
      val chars = S.toCharArray

      @scala.annotation.tailrec
      def calculate(index: Int, answer: Int, balance: Int): Int = {
        if(index == chars.length) {
          answer
        } else {
          if(chars(index) == '(') {
            calculate(index + 1, answer, balance + 1)
          } else {
            if(chars(index - 1) == '(') {
              calculate(index + 1, answer + (1 << (balance - 1)), balance - 1)
            } else {
              calculate(index + 1, answer, balance - 1)
            }
          }
        }
      }

      calculate(0, 0, 0)
    }
  }
}
