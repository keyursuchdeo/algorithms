package com.algo.arrays

object ValidPalindrome extends App {
  object Solution {
    def isPalindrome(s: String): Boolean = {
      val lowerCaseAlphaNumericS = s.toLowerCase.filter(isAlphanumeric)
      lowerCaseAlphaNumericS == lowerCaseAlphaNumericS.reverse
    }

    def isAlphanumeric(ch: Char): Boolean = {
      (ch >= 97 && ch <= 122) || (ch >= 48 && ch <= 57)
    }
  }
}
