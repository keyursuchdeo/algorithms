package com.algo.backtracking

import scala.collection.mutable

object Parentheses extends App {

  val a = Solution.generateParenthesis(3)
  println(a)

  object Solution {
    def generateParenthesis(n: Int): List[String] = {
      val maxStringLen = 2 * n
      val sb = new StringBuilder(maxStringLen)
      case class CurrStr(str: String, openingP: Int, closingP: Int)
      val stack: mutable.Stack[CurrStr] = new mutable.Stack[CurrStr]

      @scala.annotation.tailrec
      def generate(index: Int, numOfOpeningP: Int, numOfClosingP: Int, strings: Seq[String]): Seq[String] = {
        if  (index == 0) {
          sb.append("(")
          generate(index + 1, numOfOpeningP + 1, numOfClosingP, strings)
        } else if (index >= maxStringLen) {
          if(stack.nonEmpty) {
            val str = sb.toString()
            sb.clear()
            val backtrackTo = stack.pop()
            sb.append(backtrackTo.str)
            sb.append(")")
            generate(backtrackTo.str.length + 1, backtrackTo.openingP, backtrackTo.closingP + 1, str +: strings)
          } else {
            sb.toString() +: strings
          }
        } else {
          val (p, newNumOfOpeningP, newNumOfClosingP) = nextP(numOfOpeningP, numOfClosingP, sb.toString())
          sb.append(p)
          generate(index + 1, newNumOfOpeningP, newNumOfClosingP, strings)
        }
      }

      def nextP(numOfOpeningP: Int, numOfClosingP: Int, currStr: String): (String, Int, Int) = {
        if(numOfOpeningP == n){
          (")", numOfOpeningP, numOfClosingP + 1)
        } else if (numOfOpeningP == numOfClosingP){
          ("(", numOfOpeningP + 1, numOfClosingP)
        } else {
          stack.push(CurrStr(currStr, numOfOpeningP, numOfClosingP))
          ("(", numOfOpeningP + 1, numOfClosingP)
        }
      }

      generate(0, 0, 0, Nil).toList
    }
  }
}
