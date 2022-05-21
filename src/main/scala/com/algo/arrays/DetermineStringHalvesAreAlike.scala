package com.algo.arrays

object DetermineStringHalvesAreAlike extends App {
  object Solution {
    def halvesAreAlike(s: String): Boolean = {
      val chars = s.toCharArray

      def isVowel(char: Char): Boolean = {
        val lChar = char.toLower
        lChar == 'a' || lChar == 'e' || lChar == 'i' || lChar == 'o' || lChar == 'u'
      }

      @scala.annotation.tailrec
      def check(low: Int, high: Int, count1: Int, count2: Int): Boolean = {
        if (high < low) {
          count1 == count2
        } else {
          val updatedCount1 = if (isVowel(chars(low))) count1 + 1 else count1
          val updatedCount2 = if (isVowel(chars(high))) count2 + 1 else count2
          check(low + 1, high - 1, updatedCount1, updatedCount2)
        }
      }

      check(0, chars.length - 1, 0, 0)
    }
  }
}
