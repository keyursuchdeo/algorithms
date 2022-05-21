package com.algo.arrays

object CountBinarySubstrings2 extends App {

  val s = "00100"
  val res = Solution.countBinarySubstrings(s)
  println(res)

  object Solution {
    def countBinarySubstrings(s: String): Int = {
      val chars = s.toCharArray
      @scala.annotation.tailrec
      def countSubstrings(index: Int, prevCount: Int, currCount: Int, substringCount: Int): Int = {
        if(index == chars.length) {
          substringCount
        } else {
          if(chars(index) == chars(index - 1)) {
            if(prevCount == 0) {
              countSubstrings(index + 1, prevCount, currCount + 1, substringCount)
            } else {
              if(currCount + 1 <= prevCount) {
                countSubstrings(index + 1, prevCount, currCount + 1, substringCount + 1)
              } else {
                countSubstrings(index + 1, 0, currCount + 1, substringCount)
              }
            }
          } else {
            countSubstrings(index + 1, currCount, 1, substringCount + 1)
          }
        }
      }
      countSubstrings(0, 0, 0, 0)
    }
  }
}
