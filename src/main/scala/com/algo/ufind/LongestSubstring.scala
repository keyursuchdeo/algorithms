package com.algo.ufind

object LongestSubstring extends App {
  val str = "abcadcbe"
  val res = Solution.lengthOfLongestSubstring(str)
  println(res)

  object Solution {
    def lengthOfLongestSubstring(s: String): Int = {

      @scala.annotation.tailrec
      def findLength(index: Int, maxLen: Int, currLen: Int, beginIndex: Int, map: Map[Char, Int]): Int = {
        if(index == s.length) {
          maxLen
        } else {
          map.get(s(index)) match {
            case Some(prevIndex) if prevIndex >= beginIndex =>
              if(index - prevIndex > maxLen) {
                findLength(index + 1, index - prevIndex, index - prevIndex, prevIndex + 1, map + (s(index) -> index))
              } else {
                findLength(index + 1, maxLen, index - prevIndex, prevIndex + 1, map + (s(index) -> index))
              }
            case _ =>
              if(currLen + 1 > maxLen) {
                findLength(index + 1, currLen + 1, currLen + 1, beginIndex, map + (s(index) -> index))
              } else {
                findLength(index + 1, maxLen, currLen + 1, beginIndex, map + (s(index) -> index))
              }
          }
        }
      }
      findLength(0, 0, 0, 0, Map())
    }
  }
}
