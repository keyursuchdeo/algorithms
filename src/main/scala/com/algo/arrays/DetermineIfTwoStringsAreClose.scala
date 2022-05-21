package com.algo.arrays

object DetermineIfTwoStringsAreClose extends App {
  object Solution {
    def closeStrings(word1: String, word2: String): Boolean = {
      @scala.annotation.tailrec
      def letterFreq(index: Int, word: Array[Char], map: Map[Char, Int]): Map[Char, Int] = {
        if(index == word.length) {
          map
        } else {
          map.get(word(index)) match {
            case Some(freq) =>
              letterFreq(index + 1, word, map + (word(index) -> (freq + 1)))
            case None =>
              letterFreq(index + 1, word, map + (word(index) -> 1))
          }
        }
      }

      if(word1.length == word2.length) {
        val word1Chars = word1.toCharArray
        val word2Chars = word2.toCharArray
        val word1Map = letterFreq(0, word1Chars, Map())
        val word2Map = letterFreq(0, word2Chars, Map())
        word1Map.keySet == word2Map.keySet &&
          word1Map.values.toSeq.sorted == word2Map.values.toSeq.sorted
      } else {
        false
      }
    }
  }
}
