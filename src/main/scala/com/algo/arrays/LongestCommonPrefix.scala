package com.algo.arrays

object LongestCommonPrefix extends App {
  object Solution {
    def longestCommonPrefix(strs: Array[String]): String = {
      @scala.annotation.tailrec
      def find(index: Int = 1, lcp: String): String = {
        if(index == strs.length) {
          lcp
        } else {
          val newLcp = findLcpLen(lcp, strs(index))
          find(index + 1, newLcp)
        }
      }

      def findLcpLen(str1: String, str2: String): String = {
        val (small, large) = if(str1.length < str2.length) (str1, str2) else (str2, str1)
        @scala.annotation.tailrec
        def find(low: Int, high: Int, currPrefix: String): String = {
          if(high < low) {
            currPrefix
          } else {
            val mid = (low + high) / 2
            val smallSubStr = small.substring(low, mid + 1)
            if(smallSubStr == large.substring(low, mid + 1)) {
              find(mid + 1, high, s"$currPrefix$smallSubStr")
            } else {
              find(low, mid - 1, currPrefix)
            }
          }
        }
        find(0, small.length - 1, "")
      }

      if(strs.isEmpty) {
        ""
      } else {
        find(lcp = strs.head)
      }
    }
  }
}
