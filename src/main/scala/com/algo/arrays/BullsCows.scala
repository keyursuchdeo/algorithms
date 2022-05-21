package com.algo.arrays

object BullsCows extends App {
  object Solution {
    def getHint(secret: String, guess: String): String = {
      val secretChars: Array[Char] = secret.toCharArray
      val guessChars = guess.toCharArray


      def numIndex(char: Char) = {
        char - '0'
      }

      def charFreq(str: Array[Char]): Array[Int] = {
        val secretCharFreq: Array[Int] = new Array[Int](10)
        @scala.annotation.tailrec
        def fill(index: Int): Unit  = {
          if(index == secretChars.length) {
            ()
          } else {
            secretCharFreq(numIndex(str(index))) = secretCharFreq(numIndex(str(index))) + 1
            fill(index + 1)
          }
        }

        fill(0)
        secretCharFreq
      }

      @scala.annotation.tailrec
      def countBulls(index: Int, bulls: Int, secretCharFreq: Array[Int]): (Int, Array[Int]) = {
        if(index == secretChars.length) {
          (bulls, secretCharFreq)
        } else {
          if(secretChars(index) == guessChars(index)) {
            secretCharFreq(numIndex(guessChars(index))) = secretCharFreq(numIndex(guessChars(index))) - 1
            countBulls(index + 1, bulls + 1, secretCharFreq)
          } else {
            countBulls(index + 1, bulls, secretCharFreq)
          }
        }
      }

      @scala.annotation.tailrec
      def countCows(index: Int, cows: Int, secretCharFreq: Array[Int]): Int = {
        if(index == secretChars.length) {
          cows
        } else {
          if(secretChars(index) == guessChars(index) || secretCharFreq(numIndex(guessChars(index))) == 0) {
            countCows(index + 1, cows, secretCharFreq)
          } else {
            secretCharFreq(numIndex(guessChars(index))) = secretCharFreq(numIndex(guessChars(index))) - 1
            countCows(index + 1, cows + 1, secretCharFreq)
          }
        }
      }

      val secCharFreq = charFreq(secretChars)
      val (bulls, updatedSecCharFreq) = countBulls(0, 0, secCharFreq)
      val cows = countCows(0, 0, updatedSecCharFreq)

      s"${bulls}A${cows}B"
    }
  }
}
