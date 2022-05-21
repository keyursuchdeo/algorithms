package com.algo.dp


object Lcs4 extends App {
  val text1 = "bmvcnwrmxcfcxabkxcvgbozmpspsbenazglyxkpibgzq"
  val text2 = "bmpmlstotylonkvmhqjyxmnqzctonqtobahcrcbibgzgx"
  val res = Solution.longestCommonSubsequence(text1, text2)
  println(res)

  object Solution {
    def longestCommonSubsequence(text1: String, text2: String): Int = {
      val array: Array[Array[Int]] = Array.fill(text1.length, text2.length)(-1)

      def find(t1Index: Int, t2Index: Int): Int = {
        if (t1Index == text1.length || t2Index == text2.length) {
          0
        } else if (array(t1Index)(t2Index) != -1) {
          array(t1Index)(t2Index)
        } else {
          if (text1(t1Index) == text2(t2Index)) {
            val v = 1 + find(t1Index + 1, t2Index + 1)
            array(t1Index)(t2Index) = v
            v
          } else {
            val v = Math.max(
              find(t1Index, t2Index + 1),
              find(t1Index + 1, t2Index)
            )
            array(t1Index)(t2Index) = v
            v
          }
        }
      }

      find(0, 0)
    }
  }

}
