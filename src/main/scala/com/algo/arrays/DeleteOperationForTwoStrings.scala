package com.algo.arrays

object DeleteOperationForTwoStrings extends App {
  object Solution {
    def minDistance(word1: String, word2: String): Int = {

      def recordCharFreq(word: String): Map[Char, Int] = {
        val chars = word.toCharArray
        @scala.annotation.tailrec
        def record(index: Int, map: Map[Char, Int]): Map[Char, Int] = {
          if(index == chars.length) {
            map
          } else {
            record(index + 1, map + (chars(index) -> (map.getOrElse(chars(index), 0) + 1)))
          }
        }
        record(0, Map())
      }

      def recordOrRemoveCharFreq(word: String, map: Map[Char, Int]): Map[Char, Int] = {
        val chars = word.toCharArray
        @scala.annotation.tailrec
        def recordOrRemove(index: Int, currMap: Map[Char, Int]): Map[Char, Int] = {
          if(index == chars.length) {
            currMap
          } else {
            currMap.get(chars(index)) match {
              case Some(freq) if freq > 1 =>
                recordOrRemove(index + 1, currMap + (chars(index) -> (freq - 1)))
              case Some(freq) if freq == 1 =>
                recordOrRemove(index + 1, currMap - chars(index))
              case _ =>
                recordOrRemove(index + 1, currMap + (chars(index) -> 1))
            }
          }
        }

        recordOrRemove(0, map)
      }

      val freqWord1 = recordCharFreq(word1)
      val remainingFreq = recordOrRemoveCharFreq(word2, freqWord1)
      remainingFreq.values.sum
    }
  }
}
