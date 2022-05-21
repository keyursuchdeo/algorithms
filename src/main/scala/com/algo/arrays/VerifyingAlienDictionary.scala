package com.algo.arrays

object VerifyingAlienDictionary extends App {
  object Solution {
    def isAlienSorted(words: Array[String], order: String): Boolean = {
      val orderMap: Map[Char, Int] = order.zipWithIndex.toMap

      def compareWords(word1: String, word2: String): Int = {
        val word1Chars = word1.toCharArray
        val word2Chars = word2.toCharArray
        val limit = Math.min(word1Chars.length, word2Chars.length)

        @scala.annotation.tailrec
        def compare(index: Int): Int = {
          if(index == limit) {
            word1Chars.length - word2Chars.length
          } else {
            if(word1Chars(index) == word2Chars(index)) {
              compare(index + 1)
            } else {
              orderMap(word1Chars(index)) - orderMap(word2Chars(index))
            }
          }
        }

        compare(0)
      }

      @scala.annotation.tailrec
      def check(index: Int = 1): Boolean = {
        if(index == words.length) {
          true
        } else {
          if(compareWords(words(index - 1), words(index)) > 1 ) {
            false
          } else {
            check(index + 1)
          }
        }
      }

      check()
    }
  }
}
