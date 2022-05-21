package com.algo.stack

object RomanToInteger extends App {

  object Solution {
    def romanToInt(s: String): Int = {
      val chars = s.toCharArray
      val charNums: Map[Char, Int] = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

      def handleSubtraction(index: Int, subChar: Int) = {
        if (chars(index - 1) == subChar) {
          charNums(chars(index)) - 2 * charNums(chars(index - 1))
        } else {
          charNums(chars(index))
        }
      }

      @scala.annotation.tailrec
      def calculate(index: Int, num: Int): Int = {
        if (index == chars.length) {
          num
        } else {
          if (chars(index) == 'V' || chars(index) == 'X') {
            calculate(index + 1, num + handleSubtraction(index, 'I'))
          } else if (chars(index) == 'L' || chars(index) == 'C') {
            calculate(index + 1, num + handleSubtraction(index, 'X'))
          } else if (chars(index) == 'D' || chars(index) == 'M') {
            calculate(index + 1, num + handleSubtraction(index, 'C'))
          } else {
            calculate(index + 1, num + charNums(chars(index)))
          }
        }
      }

      calculate(1, charNums(chars.head))
    }
  }

}
