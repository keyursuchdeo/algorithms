package com.algo.arrays

object LongestWordInDictionaryThroughDeleting extends App {
  object Solution {
    def findLongestWord(s: String, d: List[String]): String = {

      val sChars = s.toCharArray

      def isSubsequenceOfS(dictionaryWord: Array[Char]): Boolean = {
        @scala.annotation.tailrec
        def check(dwIndex: Int, sIndex: Int): Boolean = {
          if(dwIndex == dictionaryWord.length) {
            true
          } else if (sIndex == sChars.length) {
            false
          } else {
            if(dictionaryWord(dwIndex) == sChars(sIndex)) {
              check(dwIndex + 1, sIndex + 1)
            } else {
              check(dwIndex, sIndex + 1)
            }
          }
        }
        check(0, 0)
      }

      @scala.annotation.tailrec
      def find(currD: List[String], longestWord: String, longestWordLen: Int): String = {
        if(currD.isEmpty) {
          longestWord
        } else {
          val dWordChars = currD.head.toCharArray
          if (isSubsequenceOfS(dWordChars)) {
            if (dWordChars.length > longestWordLen ||
              (dWordChars.length == longestWordLen && currD.head < longestWord)) {
              find(currD.tail, currD.head, dWordChars.length)
            }  else {
              find(currD.tail, longestWord, longestWordLen)
            }
          } else {
            find(currD.tail, longestWord, longestWordLen)
          }
        }
      }

      find(d, "", 0)
    }
  }
}
