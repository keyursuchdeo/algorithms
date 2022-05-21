package com.algo.dp

object SmallestStringWithAGivenNumericValue extends App {
  object Solution {
    def getSmallestString(n: Int, k: Int): String = {

      val output = Array.fill[Char](n)('a')

      def getChar(remainingK: Int): Char = {
        ('a' + (remainingK - 1)).toChar
      }

      @scala.annotation.tailrec
      def calculate(index: Int, remainingK: Int): Unit = {
        if(remainingK > 25) {
          output(index) = 'z'
          calculate(index - 1, remainingK - 25)
        } else {
          output(index) = getChar(remainingK)
        }
      }

      output.mkString("")
    }
  }
}
