package com.algo.stack

object EvaluateReversePolishNotation extends App {
  object Solution {
    def evalRPN(tokens: Array[String]): Int = {

      def isOperator(token: String): Boolean =
        token == "+" || token == "-" || token == "*" || token == "/"

      def evaluateWithOperator(num1: Int, num2: Int, op: String): Int = {
        if(op == "+") {
          num1 + num2
        } else if (op == "-") {
          num1 - num2
        } else if (op == "*") {
          num1 * num2
        } else {
          num1 / num2
        }
      }

      @scala.annotation.tailrec
      def evaluateExpression(index: Int, nums: Seq[Int]): Int = {
        if(index == tokens.length) {
          nums.head
        } else {
          val token = tokens(index)
          if(isOperator(token)) {
            val res = evaluateWithOperator(nums.tail.head, nums.head, token)
            evaluateExpression(index + 1, res +: nums.tail.tail)
          } else {
            evaluateExpression(index + 1, token.toInt +: nums)
          }
        }
      }

      evaluateExpression(0, Nil)
    }
  }
}
