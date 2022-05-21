package com.algo.arrays

object BasicCalculatorII extends App {
  object Solution {
    def calculate(s: String): Int = {
      val chars = s.toCharArray

      def isValidSign(char: Char): Boolean = char == '+' || char == '-' || char == '*' || char == '/'

      @scala.annotation.tailrec
      def parseExpression(index: Int, currNum: Int, parsedExpression: Seq[String]): Seq[String] = {
        if(index == chars.length) {
          (currNum.toString +: parsedExpression).reverse
        } else {
          if (chars(index).isDigit) {
            parseExpression(index + 1, currNum * 10 + chars(index).asDigit, parsedExpression)
          } else if(isValidSign(chars(index))) {
            parseExpression(index + 1, 0, chars(index).toString +: (currNum.toString +: parsedExpression))
          } else {
            parseExpression(index + 1, currNum, parsedExpression)
          }
        }
      }

      @scala.annotation.tailrec
      def solveExpressionForDivMul(expression: Seq[String], simplifiedExpression: Seq[String]):  Seq[String] = {
        if(expression.isEmpty) {
          simplifiedExpression.reverse
        } else {
          if(expression.head == "*") {
            val result: Int = simplifiedExpression.head.toInt * expression.tail.head.toInt
            solveExpressionForDivMul(expression.tail.tail, result.toString +: simplifiedExpression.tail)
          } else if(expression.head == "/") {
            val result: Int = simplifiedExpression.head.toInt / expression.tail.head.toInt
            solveExpressionForDivMul(expression.tail.tail, result.toString +: simplifiedExpression.tail)
          } else {
            solveExpressionForDivMul(expression.tail, expression.head +: simplifiedExpression)
          }
        }
      }

      @scala.annotation.tailrec
      def solveExpressionForPlusMinus(expression: Seq[String], simplifiedExpression: Seq[String]):  Seq[String] = {
        if(expression.isEmpty) {
          simplifiedExpression
        } else {
          if(expression.head == "+") {
            val result: Int = simplifiedExpression.head.toInt + expression.tail.head.toInt
            solveExpressionForPlusMinus(expression.tail.tail, result.toString +: simplifiedExpression.tail)
          } else if(expression.head == "-") {
            val result: Int = simplifiedExpression.head.toInt - expression.tail.head.toInt
            solveExpressionForPlusMinus(expression.tail.tail, result.toString +: simplifiedExpression.tail)
          } else {
            solveExpressionForPlusMinus(expression.tail, expression.head +: simplifiedExpression)
          }
        }
      }

      val parsedExpression = parseExpression(0, 0, Nil)
      val se1 = solveExpressionForDivMul(parsedExpression, Nil)
      solveExpressionForPlusMinus(se1, Nil).head.toInt
    }
  }
}
