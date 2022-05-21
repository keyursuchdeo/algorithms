package com.algo.arrays

object LongestSubstringKRepeatingChars extends App {

  val res = Solution.longestSubstring("bbaaacbd", 3)
  println(res)

  object Solution {
    def longestSubstring(s: String, k: Int): Int = {
      val chars = s.toCharArray
      val charFreq = new Array[Int](26)

      def getCharIndex(char: Char) = char - 'a'

      @scala.annotation.tailrec
      def fillCharFrequency(index: Int): Unit = {
        if(index == chars.length) {
          ()
        } else {
          charFreq(getCharIndex(chars(index))) = charFreq(getCharIndex(chars(index))) + 1
          fillCharFrequency(index + 1)
        }
      }

      def isFrequencyOfAllSeenCharsGreaterThanK(seenChars: Set[Int], seenCharsFreq: Array[Int]): Boolean = {
        !(seenChars.exists(char => {
          seenCharsFreq(char) < k
        }))
      }

      @scala.annotation.tailrec
      def calculateLength(index: Int, currLen: Int, maxLen: Int, seenChars: Set[Int], seenCharsFreq: Array[Int]): Int = {
        if(index == chars.length) {
          if(isFrequencyOfAllSeenCharsGreaterThanK(seenChars, seenCharsFreq)) {
            Math.max(currLen, maxLen)
          } else {
            maxLen
          }
        } else {
          if(charFreq(getCharIndex(chars(index))) < k) {
            if(isFrequencyOfAllSeenCharsGreaterThanK(seenChars, seenCharsFreq)) {
              calculateLength(index + 1, 0, Math.max(maxLen, currLen), Set(), new Array[Int](26))
            } else {
              calculateLength(index + 1, 0, maxLen, Set(), new Array[Int](26))
            }
          } else {
            seenCharsFreq(getCharIndex(chars(index))) = seenCharsFreq(getCharIndex(chars(index))) + 1
            calculateLength(index + 1, currLen + 1, maxLen, seenChars + getCharIndex(chars(index)), seenCharsFreq)
          }
        }
      }

      if(k == 1) {
        chars.length
      } else if(chars.length < k) {
        0
      } else {
        fillCharFrequency(0)
        calculateLength(0, 0, 0, Set(), new Array[Int](26))
      }
    }
  }
}
