package com.algo.arrays

object LongestSubstringKRepeatingChars2 extends App {

  val res = Solution.longestSubstring("aba", 2)
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

      def updateCharFrequency(currCharFreq: Array[Int], updatedCharFreq: Array[Int]): Array[Int] = {
        val updatedArray = new Array[Int](26)
        @scala.annotation.tailrec
        def update(index: Int): Unit = {
          if(index == currCharFreq.length) {
            ()
          } else {
            updatedArray(index) = currCharFreq(index) - updatedCharFreq(index)
            update(index + 1)
          }
        }

        update(0)
        updatedArray
      }

      def calculateLength(currIndex: Int, low: Int, high: Int, currCharFreq: Array[Int], updatedCharFreq: Array[Int]): Int = {
        if(currIndex == high) {
          high - low
        } else {
          if(currCharFreq(getCharIndex(chars(currIndex))) < k) {

            Math.max(calculateLength(low, low, currIndex, updatedCharFreq, new Array[Int](26)),
              calculateLength(currIndex + 1, currIndex + 1, high, updateCharFrequency(currCharFreq, updatedCharFreq), new Array[Int](26)))
          } else {
            updatedCharFreq(getCharIndex(chars(currIndex))) = updatedCharFreq(getCharIndex(chars(currIndex))) + 1
            calculateLength(currIndex + 1, low, high, currCharFreq, updatedCharFreq)
          }
        }
      }


      if(k == 1) {
        chars.length
      } else if(chars.length < k) {
        0
      } else {
        fillCharFrequency(0)
        calculateLength(0, 0, chars.length, charFreq, new Array[Int](26))
      }
    }
  }
}
