package com.algo.dp

object IsSubsequence2 extends App {

  val st = "axc"
  val tt = "ahbgdc"
  val res = Solution.isSubsequence(st, tt)
  println(res)

  object Solution {
    def isSubsequence(s: String, t: String): Boolean = {

      def check(sIndex: Int, tIndex: Int): Int = {
        if (sIndex == s.length || tIndex == t.length) {
          0
        } else {
          if (s(sIndex) == t(tIndex)) {
            1 + check(sIndex + 1, tIndex + 1)
          } else {
            check(sIndex, tIndex + 1)
          }
        }
      }

      check(sIndex = 0, tIndex = 0) == s.length
    }
  }

}
