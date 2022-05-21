package com.algo.dp

object LongestPalindromicSubsequence extends App {

  val res = Solution.longestPalindromeSubseq("robor")
  print(res)

  object Solution {
    def longestPalindromeSubseq(s: String): Int = {
      val chars = s.toCharArray
      val palindromeLen = Array.ofDim[Int](s.length, s.length)

      @scala.annotation.tailrec
      def fill(index: Int): Unit = {
        if(index == s.length) {
          ()
        } else {
          (index to 0 by - 1).foreach(i => {
            if(i == index) {
              palindromeLen(i)(index) = 1
            } else if(chars(i) == chars(index)) {
              palindromeLen(i)(index) = 2 + palindromeLen(i + 1)(index - 1)
            } else {
              palindromeLen(i)(index) = Math.max(palindromeLen(i + 1)(index), palindromeLen(i)(index - 1))
            }
          })
          fill(index + 1)
        }
      }

      fill(0)
      println(palindromeLen.map(_.mkString(",")).mkString("|"))
      palindromeLen.head(s.length - 1)


    }
  }
}
