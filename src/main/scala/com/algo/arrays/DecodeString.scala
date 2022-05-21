package com.algo.arrays

object DecodeString extends App {

  val res = Solution.decodeString("3[z]2[2[y]pq]")
  println(res)

  object Solution {
    def decodeString(s: String): String = {
      val chars = s.toCharArray

      def isNum(char: Char): Boolean = char.isDigit
      def isLetter(char: Char): Boolean = char.isLetter
      def isOpeningBracket(char: Char): Boolean = char == '['
      def isClosingBracket(char: Char): Boolean = char == ']'
      def repeatString(str: String, freq: Int): String =str * freq

      def prepString(stackedChars: Seq[Char]): Seq[Char] = {
        @scala.annotation.tailrec
        def prep(currStackedChars: Seq[Char], currStrChars: Seq[Char], currNumDigits: Seq[Char]): Seq[Char]  = {
          currStackedChars.headOption match {
            case None =>
              if(currNumDigits.isEmpty) {
                currStrChars.reverse
              } else {
                repeatString(currStrChars.reverse.mkString(""), currNumDigits.mkString("").toInt).toCharArray.toSeq
              }
            case Some(c) if isOpeningBracket(c) && currNumDigits.isEmpty =>
              prep(currStackedChars.tail, currStrChars, currNumDigits)
            case Some(c) if isOpeningBracket(c) =>
              repeatString(currStrChars.reverse.mkString(""), currNumDigits.mkString("").toInt).toCharArray ++ currStackedChars
            case Some(c) if isLetter(c) && currNumDigits.isEmpty =>
              prep(currStackedChars.tail, c +: currStrChars, currNumDigits)
            case Some(c) if isLetter(c) =>
              repeatString(currStrChars.reverse.mkString(""), currNumDigits.mkString("").toInt).toCharArray ++ currStackedChars
            case Some(c) if isNum(c) =>
              prep(currStackedChars.tail, currStrChars, c +: currNumDigits)
          }
        }
        prep(stackedChars, Nil, Nil)
      }

      @scala.annotation.tailrec
      def decode(index: Int, stackedChars: Seq[Char]): String = {
        if(index == chars.length) {
          stackedChars.reverse.mkString("")
        } else {
          if(isClosingBracket(chars(index))) {
            decode(index + 1, prepString(stackedChars))
          } else {
            decode(index + 1, chars(index) +: stackedChars)
          }
        }
      }

      decode(0, Nil)
    }
  }
}
