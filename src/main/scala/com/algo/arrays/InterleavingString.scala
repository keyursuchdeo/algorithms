package com.algo.arrays

object InterleavingString extends App {
  object Solution {
    def isInterleave(s1: String, s2: String, s3: String): Boolean = {
      val s1Chars = s1.toCharArray
      val s2Chars = s2.toCharArray
      val s3Chars = s2.toCharArray

      def check(s1Index: Int, s2Index: Int, s3Index: Int): Boolean = {
        if(s3Index == s3Chars.length) {
          true
        } else {
          if(s1Index == s1Chars.length && s2Index == s2Chars.length) {
            false
          } else if(s1Index == s1Chars.length) {
            if (s3Chars(s3Index) == s2Chars(s2Index)) {
              check(s1Index, s2Index + 1, s3Index + 1)
            } else {
              false
            }
          } else if (s2Index == s2Chars.length) {
            if (s3Chars(s3Index) == s1Chars(s1Index)) {
              check(s1Index + 1, s2Index, s3Index + 1)
            } else {
              false
            }
          }else if(s3Chars(s3Index) == s1Chars(s1Index) && s3Chars(s3Index) == s2Chars(s2Index)) {
            check(s1Index + 1, s2Index, s3Index + 1) || check(s1Index, s2Index + 1, s3Index + 1)
          } else if (s3Chars(s3Index) == s1Chars(s1Index)) {
            check(s1Index + 1, s2Index, s3Index + 1)
          } else if (s3Chars(s3Index) == s2Chars(s2Index)) {
            check(s1Index, s2Index + 1, s3Index + 1)
          } else {
            false
          }
        }
      }

      check(0, 0, 0)
    }
  }
}
