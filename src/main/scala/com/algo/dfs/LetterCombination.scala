package com.algo.dfs

object LetterCombination extends App {
  object Solution {
    def letterCombinations(digits: String): List[String] = {
      val charCombinations = new Array[Seq[Char]](10)

      def fillCharCombinations(): Unit = {
        charCombinations(0) = Nil
        charCombinations(1) = Nil
        charCombinations(2) = Seq('a', 'b', 'c')
        charCombinations(3) = Seq('d', 'e', 'f')
        charCombinations(4) = Seq('g', 'h', 'i')
        charCombinations(5) = Seq('j', 'k', 'l')
        charCombinations(6) = Seq('m', 'n', 'o')
        charCombinations(7) = Seq('p', 'q', 'r', 's')
        charCombinations(8) = Seq('t', 'u', 'v')
        charCombinations(9) = Seq('w', 'x', 'y', 'z')
      }

      def getNumIndex(char: Char): Int = {
        char - '0'
      }

      def formCombination(remainingDigits: String): Seq[String] = {
        if(remainingDigits.isEmpty) {
          Nil
        } else if (remainingDigits.length == 1) {
          charCombinations(getNumIndex(remainingDigits.head)).map(_.toString)
        } else {
          formCombination(remainingDigits.tail).flatMap(
            c => charCombinations(getNumIndex(remainingDigits.head)).map(_ +: c)
          )
        }
      }

      fillCharCombinations()
      formCombination(digits).toList
    }
  }
}
