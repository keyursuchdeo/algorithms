package com.algo.arrays

object FindReplacePattern extends App {
  object Solution {
    def findAndReplacePattern(words: Array[String], pattern: String): List[String] = {
      val patternChars = pattern.toCharArray

      def isMatching(word: String): Boolean = {
        val wordChars = word.toCharArray
        @scala.annotation.tailrec
        def check(index: Int, patternCharMap: Map[Char, Char], wordCharMap: Map[Char, Char]): Boolean = {
          if(index == wordChars.length) {
            true
          } else {
            val pChar = patternChars(index)
            val wChar = wordChars(index)
            patternCharMap.get(pChar) match {
              case Some(matchedWChar) if matchedWChar == wChar =>
                check(index + 1, patternCharMap, wordCharMap)
              case None if  wordCharMap.get(wChar).isEmpty =>
                check(index + 1, patternCharMap + (pChar -> wChar), wordCharMap + (wChar -> pChar))
              case _ =>
                false
            }
          }
        }
        check(0, Map(), Map())
      }

      @scala.annotation.tailrec
      def performOperation(index: Int, found: List[String]): List[String] = {
        if(index == words.length) {
          found
        } else {
          val word = words(index)
          if(isMatching(word)) {
            performOperation(index + 1, word +: found)
          } else {
            performOperation(index + 1, found)
          }
        }
      }

      performOperation(0, Nil)
    }
  }
}
