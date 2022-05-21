package com.algo.arrays

object UniqueMorseCodeWords extends App {
  object Solution {
    def uniqueMorseRepresentations(words: Array[String]): Int = {
      val morseCodes = Array(".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..")

      def charIndex(char: Char): Int = char - 'a'

      def encodeWord(word: String): String = {
        word.toCharArray.map(char => {
          morseCodes(charIndex(char))
        }).mkString("")
      }

      @scala.annotation.tailrec
      def encodeWords(index: Int, uniqueRepn: Set[String]): Int = {
        if(index == words.length) {
          uniqueRepn.size
        } else {
          val encodedRepn = encodeWord(words(index))
          encodeWords(index + 1, uniqueRepn + encodedRepn)
        }
      }

      encodeWords(0, Set())
    }
  }
}
