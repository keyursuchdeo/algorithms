package com.algo.dp

import scala.collection.mutable

object EditDistance extends App {

  val w1 = "abbbit"
  val w2 = "rabbit"
//  val w1 = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef"
//  val w2 = "bcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg"
  val res = Solution.minDistance(w1, w2)
  println(res)

  object Solution {
    def minDistance(word1: String, word2: String): Int = {
      val arr: Array[Array[Int]] = Array.fill[Int](word1.length, word2.length)(-1)
      def min(w1Index: Int, w2Index: Int): Int = {
        if(w1Index == word1.length) {
          word2.length - w2Index
        } else if (w2Index == word2.length) {
          word1.length - w1Index
        } else {
          if(arr(w1Index)(w2Index) > -1) arr(w1Index)(w2Index) else {
            if(word1(w1Index) == word2(w2Index)) {
              arr(w1Index)(w2Index) = Math.min(Math.min(min(w1Index + 1, w2Index + 1), 1 + min(w1Index, w2Index + 1)), 1 + min(w1Index + 1, w2Index))
              arr(w1Index)(w2Index)
            } else {
              arr(w1Index)(w2Index) = Math.min(Math.min(1 + min(w1Index + 1, w2Index + 1), 1 + min(w1Index, w2Index + 1)), 1 + min(w1Index + 1, w2Index))
              arr(w1Index)(w2Index)
            }
          }
        }
      }
      val out = min(0, 0)
//      println(arr.map(_.mkString(",")).mkString("|"))
      out
    }
  }
}
