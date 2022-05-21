package com.algo.arrays

import scala.util.Try

object NumberOfAtoms extends App {

  val f = "Be32"
  Solution.countOfAtoms(f)

  object Solution {
    def countOfAtoms(formula: String): String = {
      val chars = formula.toCharArray
      val atomCounts = Map[String, Int]()

      def isClosingBracket(char: Char): Boolean = char == ')'
      def isOpeningBracket(char: Char): Boolean = char == '('
      def isDigit(char: Char): Boolean = char.isDigit
      def isUppercaseLetter(char: Char): Boolean = char.isUpper
      def isLowercaseLetter(char: Char): Boolean = char.isLower

      @scala.annotation.tailrec
      def prepInputSeq(index: Int, input: Seq[String], prevString: Seq[Char]): Seq[String] = {
        if(index == chars.length) {
          if(prevString.isEmpty) {
            println(input.reverse)
            input
          } else {
            println((prevString.reverse.mkString("") +: input).reverse)
            prevString.reverse.mkString("") +: input
          }
        } else {
          if (isLowercaseLetter(chars(index))) {
            prepInputSeq(index + 1, input, chars(index) +: prevString)
          } else if (isUppercaseLetter(chars(index))) {
            if(prevString.isEmpty) {
              prepInputSeq(index + 1, input, chars(index) +: prevString)
            } else {
              prepInputSeq(index + 1, prevString.reverse.mkString("") +: input, chars(index) +: Nil)
            }
          } else if (isOpeningBracket(chars(index)) || isClosingBracket(chars(index))) {
            if(prevString.isEmpty) {
              prepInputSeq(index + 1, chars(index).toString +: input, prevString)
            } else {
              prepInputSeq(index + 1, chars(index).toString +: (prevString.reverse.mkString("") +: input), Nil)
            }
          } else {
            if(prevString.nonEmpty && prevString.head.isLetter) {
              prepInputSeq(index + 1, (prevString.reverse.mkString("") +: input), chars(index) +: Nil)
            } else {
              prepInputSeq(index + 1, input, chars(index) +: prevString)
            }
          }
        }
      }

      def isNumber(num: String) = Try(num.toInt).isSuccess

//      def count(input: Seq[String], num1: Int) {
//        if(input.isEmpty) {
//
//        } else {
//          val head = input.head
//          if(head == ")") {
//            if(num1 == 0) {
//              count(input.tail, 1)
//            } else {
//              count(input.tail, num1)
//            }
//          } else if (isNumber(head)) {
//            if(num1 == 0) {
//              count(input.tail, head.toInt)
//            } else {
//
//            }
//          } else
//
//        }
//      }

      val inputString: Seq[String] = prepInputSeq(0, Nil, Nil)


      ""
    }
  }
}
