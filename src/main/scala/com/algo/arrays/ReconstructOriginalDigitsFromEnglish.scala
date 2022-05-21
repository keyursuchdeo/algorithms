package com.algo.arrays

object ReconstructOriginalDigitsFromEnglish extends App {
  object Solution {
    def originalDigits(s: String): String = {
      val chars = s.toCharArray
      val charFreq = new Array[Int](26)
      val numFreq = new Array[Int](10)

      def getCharIndex(char: Char) = char - 'a'

      @scala.annotation.tailrec
      def recordCharFreq(index: Int): Unit = {
        if(index == chars.length) {
          ()
        } else {
          val charIndex = getCharIndex(chars(index))
          charFreq(charIndex) = charFreq(charIndex) + 1
          recordCharFreq(index + 1)
        }
      }

      def reduceCharFrequencyOf(char: Char, number: Int): Unit = {
        val charIndex = getCharIndex(char)
        charFreq(charIndex) = charFreq(charIndex) - number
      }

      def reduceFrequenciesOf(string: String, number: Int): Unit = {
        string.foreach(char => {
          reduceCharFrequencyOf(char, number)
        })
      }

      def getDigitFreqGivenChar(char: Char): Int = charFreq(getCharIndex(char))

      def recordDigitCount(): Unit = {
        val numOfSixes = getDigitFreqGivenChar('x')
        if(numOfSixes > 0) {
          numFreq(6) = numOfSixes
          reduceFrequenciesOf("six", numOfSixes)
        }

        val numOfSevens = getDigitFreqGivenChar('s')
        if(numOfSevens > 0) {
          numFreq(7) = numOfSevens
          reduceFrequenciesOf("seven", numOfSevens)
        }

        val numOfFives = getDigitFreqGivenChar('v')
        if(numOfFives > 0) {
          numFreq(5) = numOfFives
          reduceFrequenciesOf("five", numOfFives)
        }

        val numOfFours = getDigitFreqGivenChar('u')
        if(numOfFours > 0) {
          numFreq(4) = numOfFours
          reduceFrequenciesOf("four", numOfFours)
        }

        val numOfTwos = getDigitFreqGivenChar('w')
        if(numOfTwos > 0) {
          numFreq(2) = numOfTwos
          reduceFrequenciesOf("two", numOfTwos)
        }

        val numOfEights = getDigitFreqGivenChar('g')
        if(numOfEights > 0) {
          numFreq(8) = numOfEights
          reduceFrequenciesOf("eight", numOfEights)
        }

        val numOfThrees = getDigitFreqGivenChar('h')
        if(numOfThrees > 0) {
          numFreq(3) = numOfThrees
          reduceFrequenciesOf("three", numOfThrees)
        }

        val numOfZeros = getDigitFreqGivenChar('z')
        if(numOfZeros > 0) {
          numFreq(0) = numOfZeros
          reduceFrequenciesOf("zero", numOfZeros)
        }

        val numOfOnes = getDigitFreqGivenChar('o')
        if(numOfOnes > 0) {
          numFreq(1) = numOfOnes
          reduceFrequenciesOf("one", numOfOnes)
        }

        val numOfNines = getDigitFreqGivenChar('i')
        if(numOfNines > 0) {
          numFreq(9) = numOfNines
          reduceFrequenciesOf("nine", numOfNines)
        }
      }

      @scala.annotation.tailrec
      def stringifyDigits(index: Int, output: Seq[Int]): String = {
       if(index == numFreq.length) {
         output.mkString("")
       } else {
         val freq = numFreq(index)
         if(freq == 0) {
           stringifyDigits(index + 1, output)
         } else {
           val string = (0 until freq).map(_ => index)
           stringifyDigits(index + 1, output ++ string)
         }
       }
      }

      recordCharFreq(0)
      recordDigitCount()
      stringifyDigits(0, Nil)

    }
  }
}
