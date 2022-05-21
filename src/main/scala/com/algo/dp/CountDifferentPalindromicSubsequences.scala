package com.algo.dp

object CountDifferentPalindromicSubsequences extends App {
  object Solution {
    def countPalindromicSubsequences(S: String): Int = {
      val chars = S.toCharArray
      val palindromeLen = Array.ofDim[Int](S.length, S.length)

      @scala.annotation.tailrec
      def count(index: Int): Unit = {
        if(index == chars.length) {
          ()
        } else {
          (index to 0 by -1).foreach(i => {
            if(i == index) {
              palindromeLen(i)(i) = 1
            } else if (chars(i) == chars(index)) {
              palindromeLen(i)(index) = 2 + palindromeLen(i + 1)(index - 1) * 2
            } else {
              palindromeLen(i)(index) = 1 + palindromeLen(i)(index - 1)
            }
          })
          count(index + 1)
        }
      }

      count(0)
      palindromeLen.head(chars.length - 1)

    }
  }
}
