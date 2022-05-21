package com.algo.arrays

object WordSubsets extends App {
  object Solution {
    def wordSubsets(A: Array[String], B: Array[String]): List[String] = {
      def getCharIndex(char: Char) = char - 'a'

      def recordCharFrequency(word: String): Array[Int] = {
        val charFrequency: Array[Int] = new Array[Int](26)
        val wordChars: Array[Char] = word.toCharArray
        wordChars.foreach(char => {
          val charIndex = getCharIndex(char)
          charFrequency(charIndex) = charFrequency(charIndex) + 1
        })
        charFrequency
      }

      def recordCharFrequencies(words: Array[String]): Array[Array[Int]] = {
        words.map(recordCharFrequency)
      }

      val charFrequenciesB: Array[Array[Int]] = recordCharFrequencies(B)

      def isSubset(charFrequencyA: Array[Int], charFrequencyB: Array[Int]): Boolean = {
        @scala.annotation.tailrec
        def check(index: Int): Boolean = {
          if(index == charFrequencyA.length) {
            true
          } else {
            if(charFrequencyB(index) <= charFrequencyA(index)) {
              check(index + 1)
            } else {
              false
            }
          }
        }
        check(0)
      }

      def isSubsetOfB(word: String) = {
        val charFrequency = recordCharFrequency(word)
        charFrequenciesB.forall(charFrequencyB => {
          isSubset(charFrequency, charFrequencyB)
        })
      }

      @scala.annotation.tailrec
      def findSubsets(index: Int, subsets: Seq[String]): Seq[String] = {
        if(index == A.length) {
          subsets
        } else {
          if(isSubsetOfB(A(index))) {
            findSubsets(index + 1, A(index) +: subsets)
          } else {
            findSubsets(index + 1, subsets)
          }
        }
      }

      findSubsets(0, Nil).toList
    }
  }
}
