package com.algo.arrays

object ShortEncodingWords extends App {
  object Solution {
    def minimumLengthEncoding(words: Array[String]): Int = {
      object LengthOrder extends Ordering[String] {
        override def compare(x: String, y: String): Int = {
          x.length compare y.length
        }
      }

      val sortedWords = words.sorted(LengthOrder)

      def isSubstringOfLongerWords(wordIndex: Int): Boolean = {
        (0 until wordIndex).exists(index => {
          sortedWords(index).endsWith(sortedWords(wordIndex))
        })
      }

      @scala.annotation.tailrec
      def findShortestEncoding(index: Int, encodingLength: Int): Int = {
        if(index == sortedWords.length) {
          encodingLength
        } else {
          if(isSubstringOfLongerWords(index)) {
            findShortestEncoding(index + 1, encodingLength)
          } else {
            findShortestEncoding(index + 1, encodingLength + sortedWords(index).length + 1)
          }
        }
      }

      findShortestEncoding(0, 0)
    }
  }
}
