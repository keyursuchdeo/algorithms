package com.algo.dp

object Lcs2 extends App {
  val text1 = "bl"
  val text2 = "yby"
  val res = Solution.longestCommonSubsequence(text1, text2)
  println(res)
  object Solution {
    def longestCommonSubsequence(text1: String, text2: String): Int = {
      @scala.annotation.tailrec
      def subSeq(index: Int, inputText: String, remainingText: String, subSeqLens: Array[Int]): Array[Int] = {
        if(index == inputText.length) {
          subSeqLens
        } else {
          remainingText.headOption match {
            case Some(head) =>
              if(inputText(index) == head) {
                subSeqLens(index) = if (index - 1 >= 0) subSeqLens(index - 1) + 1 else 1
                subSeq(index + 1, inputText, remainingText.tail, subSeqLens)
              } else {
                subSeqLens(index) = if (index - 1 >= 0) subSeqLens(index - 1) else 0
                subSeq(index + 1, inputText, remainingText, subSeqLens)
              }
            case None =>
              subSeqLens(index) = if (index - 1 >= 0) subSeqLens(index - 1) else 0
              subSeq(index + 1, inputText, remainingText, subSeqLens)
          }
        }
      }

      if(text1.isEmpty || text2.isEmpty) {
        0
      } else {
        val updatedSubSeqLens1 = subSeq(0, text1, text2, new Array[Int](text1.length))
        val maxLen1 = updatedSubSeqLens1(text1.length - 1)
        val updatedSubSeqLens2 = subSeq(0, text2, text1, new Array[Int](text2.length))
        val maxLen2 = updatedSubSeqLens2(text2.length - 1)
        Math.max(maxLen1, maxLen2)
      }
    }
  }
}
