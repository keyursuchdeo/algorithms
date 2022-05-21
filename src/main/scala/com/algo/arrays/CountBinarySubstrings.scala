package com.algo.arrays

object CountBinarySubstrings extends App {

  val s = "00100"
  val res = Solution.countBinarySubstrings(s)
  println(res)

  object Solution {
    def countBinarySubstrings(s: String): Int = {
      val chars = s.toCharArray
      @scala.annotation.tailrec
      def countSubstrings(index: Int, zeroCount: Int, oneCount: Int, substringCount: Int): Int = {
        if(index == chars.length) {
          substringCount
        } else {
          if(zeroCount > 0 && oneCount > 0 && zeroCount == oneCount) {
            if(chars(index) == '0') {
              if(chars(index - 1) == chars(index)) {
                countSubstrings(index, zeroCount + 1, 0, substringCount)
              } else {
                countSubstrings(index, 0, oneCount, substringCount)
              }
            } else {
              if(chars(index - 1) == chars(index)) {
                countSubstrings(index, 0, oneCount + 1, substringCount)
              } else {
                countSubstrings(index, zeroCount, 0, substringCount)
              }
            }
          } else {
            if(chars(index) == '0') {
              if(zeroCount + 1 > oneCount) {
                countSubstrings(index + 1, zeroCount + 1, 0, substringCount)
              } else {
                countSubstrings(index + 1, zeroCount + 1, oneCount, substringCount + 1)
              }
            } else {
              if(oneCount + 1 > zeroCount) {
                countSubstrings(index + 1, 0, oneCount + 1, substringCount)
              } else {
                countSubstrings(index + 1, zeroCount, oneCount + 1, substringCount + 1)
              }
            }
          }
        }
      }
      countSubstrings(0, 0, 0, 0)
    }
  }
}
