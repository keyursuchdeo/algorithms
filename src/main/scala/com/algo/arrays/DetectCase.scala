package com.algo.arrays

object DetectCase extends App {

  val res = Solution.detectCapitalUse("FlaG")
  println(res)

  object Solution {
    def detectCapitalUse(word: String): Boolean = {
      def isUppercaseChar(char: Char): Boolean = {
        char >= 65 && char <= 90
      }

      def checkAllUpperCase(str: String): Boolean = {
        str.forall(char => {
          char >= 65 && char <= 90
        })
      }

      def checkAllLowerCase(str: String): Boolean = {
        str.forall(char => {
          char >= 97 && char <= 122
        })
      }

      def detect(): Boolean = {
        if(word.isEmpty || word.lengthCompare(1) == 0) {
          true
        } else {
          if (isUppercaseChar(word.head)) {
            checkAllUpperCase(word.tail) || checkAllLowerCase(word.tail)
          } else {
            checkAllLowerCase(word.tail)
          }
        }
      }

      detect()
    }
  }
}
