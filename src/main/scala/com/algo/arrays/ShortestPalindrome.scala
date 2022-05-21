package com.algo.arrays

object ShortestPalindrome extends App {
  object Solution {
    def shortestPalindrome(s: String): String = {
      val len = s.length
      val reversedS = s.reverse

      @scala.annotation.tailrec
      def find(index: Int): String = {
        if(index == len) {
          ""
        } else {
          if(s.substring(0, len - index) == reversedS.substring(index)) {
            reversedS.substring(0, index) + s
          } else {
            find(index + 1)
          }
        }
      }

      find(0)
    }
  }
}
