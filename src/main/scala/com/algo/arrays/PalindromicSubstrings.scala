package com.algo.arrays

object PalindromicSubstrings extends App {
  object Solution {
    def countSubstrings(s: String): Int = {
      val chars = s.toCharArray

      @scala.annotation.tailrec
      def isPalindrome(startIndex: Int, endIndex: Int): Boolean = {
        if(endIndex <= startIndex) {
          true
        } else {
          if(chars(endIndex) == chars(startIndex)) {
            isPalindrome(startIndex + 1, endIndex - 1)
          } else {
            false
          }
        }
      }

      def countPalindromicSubstrings(startIndex: Int, endIndex: Int): Int = {
        if(endIndex == chars.length) {
          0
        } else {
          if(isPalindrome(startIndex, endIndex)) {
            1 +
              countPalindromicSubstrings(startIndex, endIndex + 1) +
              countPalindromicSubstrings(endIndex, endIndex + 1)
          } else {
            countPalindromicSubstrings(startIndex, endIndex + 1) +
              countPalindromicSubstrings(endIndex, endIndex + 1)
          }
        }
      }

      countPalindromicSubstrings(0, 1) + chars.length

    }
  }
}
