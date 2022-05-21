package com.algo.arrays

object CheckIfTwoStringArraysAreEquivalent extends App {

  object Solution {
    def arrayStringsAreEqual(word1: Array[String], word2: Array[String]): Boolean = {

      @scala.annotation.tailrec
      def check(word1Index: Int, remainingWord1: String, word2Index: Int, remainingWord2: String): Boolean = {
        if (remainingWord1.nonEmpty && remainingWord2.nonEmpty) {
          if (remainingWord1.head == remainingWord2.head) {
            check(word1Index, remainingWord1.tail, word2Index, remainingWord2.tail)
          } else {
            false
          }
        } else if (remainingWord1.nonEmpty) {
          if(word2Index < word2.length) {
            check(word1Index, remainingWord1, word2Index + 1, word2(word2Index))
          } else {
            false
          }
        } else if (remainingWord2.nonEmpty) {
          if(word1Index < word1.length) {
            check(word1Index + 1, word1(word1Index), word2Index, remainingWord2)
          } else {
            false
          }
        } else {
          if(word1Index < word1.length && word2Index < word2.length) {
            check(word1Index + 1, word1(word1Index), word2Index + 1, word2(word2Index))
          } else {
            word1Index == word1.length && word2Index == word2.length
          }
        }
      }

      check(1, word1.head, 1, word2.head)
    }
  }

}
