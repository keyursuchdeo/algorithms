package com.algo.arrays

object LongestRepeatingCharReplacement extends App {

  val res = Solution.characterReplacement("AAAAABBBBBBB", 10)
  println(res)

  object Solution {
    def characterReplacement(s: String, k: Int): Int = {
      val charSet = s.toSet

      def countMaxRepeating(str: String): Int = {
        @scala.annotation.tailrec
        def count(prevChar: Option[Char], currStr: String, currLen: Int, maxLen: Int): Int = {
          if(currStr.isEmpty) {
            Math.max(currLen, maxLen)
          } else {
            prevChar match {
              case Some(char) if char == currStr.head =>
                count(prevChar, currStr.tail, currLen + 1, maxLen)
              case Some(char) =>
                count(currStr.headOption, currStr.tail, 1, Math.max(currLen, maxLen))
              case None =>
                count(currStr.headOption, currStr.tail, 1, 1)
            }
          }
        }

        count(None, str, 0, 0)
      }

      def find(currStr: String, remainingK: Int, headStr: String, updatedStr: String): Int = {
        if(currStr.isEmpty || remainingK == 0) {
          println(updatedStr)
          countMaxRepeating(updatedStr)
        } else {
          Math.max(
            find(currStr.tail, remainingK, headStr + currStr.head, updatedStr),
            (charSet - currStr.head).map(char => {
              find(currStr.tail, remainingK - 1, headStr + char, headStr + char + currStr.tail)
            }).max
          )
        }
      }

      find(s, k, "", s)
    }
  }

}
