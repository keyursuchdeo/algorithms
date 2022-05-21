package com.algo.arrays

import scala.util.{Success, Try}

object BasicCalculator extends App {

  val res = Solution.calculate("(1+(4+5+2)-3)")
  println(res)

  object Solution {
    def calculate(s: String): Int = {
      val chars = s.toCharArray

      def evaluate(extractedExpression: Seq[String]): String = {
        @scala.annotation.tailrec
        def solveExpressionForDivMul(ex: Seq[String], simplifiedExpression: Seq[String] = Nil):  Seq[String] = {
          if(ex.isEmpty) {
            simplifiedExpression.reverse
          } else {
            if(ex.head == "*") {
              val result: Int = simplifiedExpression.head.toInt * ex.tail.head.toInt
              solveExpressionForDivMul(ex.tail.tail, result.toString +: simplifiedExpression.tail)
            } else if(ex.head == "/") {
              val result: Int = simplifiedExpression.head.toInt / ex.tail.head.toInt
              solveExpressionForDivMul(ex.tail.tail, result.toString +: simplifiedExpression.tail)
            } else {
              solveExpressionForDivMul(ex.tail, ex.head +: simplifiedExpression)
            }
          }
        }

        @scala.annotation.tailrec
        def solveExpressionForPlusMinus(ex: Seq[String], simplifiedExpression: Seq[String] = Nil):  Seq[String] = {
          if(ex.isEmpty) {
            simplifiedExpression
          } else {
            if(ex.head == "+") {
              val result: Int = simplifiedExpression.head.toInt + ex.tail.head.toInt
              solveExpressionForPlusMinus(ex.tail.tail, result.toString +: simplifiedExpression.tail)
            } else if(ex.head == "-") {
              val result: Int = simplifiedExpression.head.toInt - ex.tail.head.toInt
              solveExpressionForPlusMinus(ex.tail.tail, result.toString +: simplifiedExpression.tail)
            } else {
              solveExpressionForPlusMinus(ex.tail, ex.head +: simplifiedExpression)
            }
          }
        }

        solveExpressionForPlusMinus(solveExpressionForDivMul(extractedExpression)).head
      }

      def evaluateExpressionUntilOpeningParen(expression: Seq[String]): (Int, Seq[String]) = {
        @scala.annotation.tailrec
        def extractExpressionUntilOpeningParen(currExpression: Seq[String], extractedExpression: Seq[String]): (Seq[String], Seq[String]) = {
          currExpression.headOption match {
            case None => (Nil, extractedExpression)
            case Some("(") => (currExpression.tail, extractedExpression)
            case Some(str) => extractExpressionUntilOpeningParen(currExpression.tail, str +: extractedExpression)
          }
        }

        val (remainingExpression, expToEvaluate) = extractExpressionUntilOpeningParen(expression, Nil)
        (evaluate(expToEvaluate).toInt, remainingExpression)
      }

      @scala.annotation.tailrec
      def parseExpression(index: Int, currNum: Int, expression: Seq[String]): Seq[String] = {
        if (index == chars.length) {
          if(expression.isEmpty) {
            Seq(currNum.toString)
          } else {
            if(expression.head == "+" || expression.head == "-") {
              (currNum.toString +: expression).reverse
            } else {
              expression.reverse
            }
          }
        } else {
          if (chars(index).isDigit) {
            parseExpression(index + 1, currNum * 10 + chars(index).asDigit, expression)
          } else if (chars(index) == '(') {
            parseExpression(index + 1, 0, "(" +: expression)
          } else if (chars(index) == ')') {
            println(currNum.toString +: expression)
            val (result, updatedExpression) = evaluateExpressionUntilOpeningParen(currNum.toString +: expression)
            println(updatedExpression)
            parseExpression(index + 1, result, updatedExpression)
          } else if (chars(index) == '+' || chars(index) == '-') {
            parseExpression(index + 1, 0, chars(index).toString +: (currNum.toString +: expression))
          } else {
            parseExpression(index + 1, currNum, expression)
          }
        }
      }


      val parsedExpression = parseExpression(0, 0, Nil)
      evaluate(parsedExpression).toInt
    }
  }

}
