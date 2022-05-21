package com.algo.arrays

object GenerateParentheses extends App {
  object Solution {
    def generateParenthesis(n: Int): List[String] = {
      def wrap(parenthesis: String): String = {
        s"($parenthesis)"
      }

      def adjacent(parenthesis: String): List[String] = {
        val left = s"()$parenthesis"
        val right = s"$parenthesis()"
        if(left == right) List(left) else List(left, right)
      }

      def generate(currN: Int): List[String] = {
        if(currN == 1) {
          List("()")
        } else {
          generate(currN - 1).flatMap(parenthesis => {
            wrap(parenthesis) +: adjacent(parenthesis)
          })
        }
      }

      generate(n)
    }
  }
}
