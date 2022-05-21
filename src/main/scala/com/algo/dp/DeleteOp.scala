package com.algo.dp

import scala.collection.mutable

object DeleteOp extends App {

  val input1 = "dinitrophenylhydrazine"
  val input2 = "acetylphenylhydrazine"
  val res = Solution.minDistance(input1, input2)
  println(res)

  object Solution {
    def minDistance(word1: String, word2: String): Int = {

      val array: Array[Array[Int]] = Array.fill(word1.length, word2.length)(-1)
      def lcs(t1Index: Int, t2Index: Int): Int = {
        if (t1Index == word1.length || t2Index == word2.length) {
          0
        } else if (array(t1Index)(t2Index) != -1) {
          array(t1Index)(t2Index)
        } else {
          if (word1(t1Index) == word2(t2Index)) {
            val v = 1 + lcs(t1Index + 1, t2Index + 1)
            array(t1Index)(t2Index) = v
            v
          } else {
            val v = Math.max(
              lcs(t1Index, t2Index + 1),
              lcs(t1Index + 1, t2Index)
            )
            array(t1Index)(t2Index) = v
            v
          }
        }
      }

      val lcsLen = lcs(0, 0)
      word1.length + word2.length - 2 * lcsLen
    }
  }
}
