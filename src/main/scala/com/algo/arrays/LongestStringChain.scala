package com.algo.arrays

object LongestStringChain extends App {

  object Solution {
    def longestStrChain(words: Array[String]): Int = {

      val longestChainLen = Array.fill[Int](words.length)(-1)

      @scala.annotation.tailrec
      def mapLength(index: Int, map: Map[Int, Seq[Int]]): Map[Int, Seq[Int]] = {
        if (index == words.length) {
          map
        } else {
          val len = words(index).length
          map.get(len) match {
            case Some(indices) =>
              mapLength(index + 1, map + (len -> (index +: indices)))
            case None =>
              mapLength(index + 1, map + (len -> (index +: Nil)))
          }
        }
      }

      val lenMap: Map[Int, Seq[Int]] = mapLength(0, Map())

      def charFreq(word: String) = {
        val chars = word.toCharArray

        @scala.annotation.tailrec
        def calculate(index: Int, freq: Map[Char, Int]): Map[Char, Int] = {
          if (index == chars.length) {
            freq
          } else {
            freq.get(chars(index)) match {
              case Some(value) =>
                calculate(index + 1, freq + (chars(index) -> (value + 1)))
              case None =>
                calculate(index + 1, freq + (chars(index) -> 1))
            }
          }
        }

        calculate(0, Map())
      }

      def isSuccessor1(word1: String, word2: String): Boolean = {
        val word1Chars = word1.toCharArray
        val word2Chars = word2.toCharArray
        @scala.annotation.tailrec
        def check(word1Index: Int, word2Index: Int, mismatchFound: Boolean): Boolean = {
          if(word1Index == word1Chars.length) {
            true
          } else {
            if(word2Chars(word2Index) == word1Chars(word1Index)) {
              check(word1Index + 1, word2Index + 1, mismatchFound)
            } else {
              if(mismatchFound) {
                false
              } else {
                check(word1Index, word2Index + 1, mismatchFound = true)
              }
            }
          }
        }
        check(0, 0, mismatchFound = false)
      }

      @scala.annotation.tailrec
      def findLongestChain(index: Int): Int = {
        if (index == words.length) {
          0
        } else {
          findLongest(index)
          findLongestChain(index + 1)
        }
      }

      def findLongest(index: Int): Int = {
        if (longestChainLen(index) > -1) {
          longestChainLen(index)
        } else {
          val word = words(index)
          val possibleSuccessors = lenMap.getOrElse(word.length + 1, Nil)
          longestChainLen(index) =
            if (possibleSuccessors.isEmpty) {
              0
            } else {
              val a =
                possibleSuccessors.map(successor => {
                  if (isSuccessor1(word, words(successor))) {
                    1 + findLongest(successor)
                  } else {
                    1
                  }
                })
              a.max
            }
          longestChainLen(index)
        }
      }

      findLongestChain(0)
      longestChainLen.max
    }
  }

}
