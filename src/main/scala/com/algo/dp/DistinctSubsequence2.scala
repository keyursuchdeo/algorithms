package com.algo.dp

object DistinctSubsequence2 extends App {

  object Solution {
    def numDistinct(s: String, t: String): Int = {
      val countsByStrings = Array.fill[Int](s.length, t.length)(-1)

      def count(sIndex: Int, tIndex: Int): Int = {
        if (tIndex == t.length) {
          1
        } else if (sIndex == s.length) {
          0
        } else {
          if(countsByStrings(sIndex)(tIndex) != -1) {
            countsByStrings(sIndex)(tIndex)
          } else {
            val currCount =
              if (s(sIndex) == t(tIndex)) {
                count(sIndex + 1, tIndex + 1) + count(sIndex + 1, tIndex)
              } else {
                count(sIndex + 1, tIndex)
              }
            countsByStrings(sIndex)(tIndex) = currCount
            currCount
          }
        }
      }

      count(0, 0)
    }
  }

}
