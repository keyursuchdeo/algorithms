package com.algo.arrays

object ToLowerCase extends App {
  object Solution {
    def toLowerCase(s: String): String = {
      val chars = s.toCharArray
      val charDiff: Int = 'a' - 'A'
      def isUpper(char: Char): Boolean = {
        char.isUpper
      }

      @scala.annotation.tailrec
      def changeCase(index: Int): Unit = {
        if(index == chars.length) {
          ()
        } else {
          if(isUpper(chars(index))) {
            val num: Int = chars(index) + charDiff
            chars(index) = num.toChar
            changeCase(index + 1)
          } else {
            changeCase(index + 1)
          }
        }
      }

      changeCase(0)
      chars.mkString
    }
  }
}
