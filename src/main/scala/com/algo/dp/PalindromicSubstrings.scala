package com.algo.dp

object PalindromicSubstrings extends App {
  object Solution {
    def countSubstrings(s: String): Int = {
      val substringCounts = new Array[Int](s.length)

      @scala.annotation.tailrec
      def count(index: Int, checkedString: String, remainingString: String): Unit = {
        if(remainingString.isEmpty) {
          ()
        } else {
          val currStr = checkedString + remainingString.head
          substringCounts(index) = findSubstrings(currStr, 0)
          count(index + 1, currStr, remainingString.tail)
        }
      }

      @scala.annotation.tailrec
      def findSubstrings(currString: String, count: Int): Int = {
        if(currString.isEmpty) {
          count
        } else {
          if(currString == currString.reverse) {
            findSubstrings(currString.tail, count + 1)
          } else {
            findSubstrings(currString.tail, count)
          }
        }
      }

      count(0, "", s)
      substringCounts.sum
    }
  }
}
