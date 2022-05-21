package com.algo.dp

object IsSubsequence extends App {

  val st = "axc"
  val tt = "ahbgdc"
  val res = Solution.isSubsequence(st, tt)
  println(res)

  object Solution {
    def isSubsequence(s: String, t: String): Boolean = {
      val array: Array[Array[Int]] = Array.fill[Int](s.length, t.length)(-1)

      def check(sIndex: Int, tIndex: Int): Int = {
        if (sIndex == s.length || tIndex == t.length) {
          0
        } else if (array(sIndex)(tIndex) != -1) {
          array(sIndex)(tIndex)
        } else {
              val output =
                if (s(sIndex) == t(tIndex)) {
                  1 + check(sIndex + 1, tIndex + 1)
                } else {
                  Math.max(
                    check(sIndex, tIndex + 1),
                    check(sIndex + 1, tIndex)
                  )
                }
              array(sIndex)(tIndex) = output
              output
        }
      }

      check(sIndex = 0, tIndex = 0) == s.length
    }
  }

}
