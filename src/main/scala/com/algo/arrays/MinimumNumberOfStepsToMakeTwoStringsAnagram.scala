package com.algo.arrays

object MinimumNumberOfStepsToMakeTwoStringsAnagram extends App {
  object Solution {
    def minSteps(s: String, t: String): Int = {
      val sChars = s.toCharArray
      val tChars = t.toCharArray
      val charFreq = new Array[Int](26)

      def getCharIndex(char: Char): Int = char - 'a'

      @scala.annotation.tailrec
      def calculateCharFreqOfS(index: Int): Unit = {
        if(index == sChars.length) {
          ()
        } else {
          val charIndex = getCharIndex(sChars(index))
          charFreq(charIndex) = charFreq(charIndex) + 1
          calculateCharFreqOfS(index + 1)
        }
      }

      @scala.annotation.tailrec
      def reduceCharFreqOfSBasedOnT(index: Int): Unit = {
        if(index == tChars.length) {
          ()
        } else {
          val charIndex = getCharIndex(tChars(index))
          if(charFreq(charIndex) > 0) {
            charFreq(charIndex) = charFreq(charIndex) - 1
          }
          reduceCharFreqOfSBasedOnT(index + 1)
        }
      }

      calculateCharFreqOfS(0)
      reduceCharFreqOfSBasedOnT(0)
      charFreq.sum
    }
  }
}
