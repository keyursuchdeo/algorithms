package com.algo.dp

object LongestPalindrome extends App {
  object Solution {
    def longestPalindrome(s: String): Int = {

      object FreqOrder extends Ordering[(Char, Int)] {
        override def compare(x: (Char, Int), y: (Char, Int)): Int = {
          val (_, xF) = x
          val (_, yF) = y
          yF compare xF
        }
      }

      val charFreqQueue = scala.collection.mutable.PriorityQueue.empty(FreqOrder)

      @scala.annotation.tailrec
      def recordCharFreq(currS: String, map: Map[Char, Int]): Map[Char, Int] = {
        if(currS.isEmpty) {
          map
        } else {
          map.get(currS.head) match {
            case Some(value) =>
              recordCharFreq(currS.tail, map + (currS.head -> (value + 1)))
            case None =>
              recordCharFreq(currS.tail, map + (currS.head -> 1))
          }
        }
      }

      def fillQueue(map: Map[Char, Int]): Unit = {
        charFreqQueue.enqueue(map.toSeq: _*)
      }

      def calculateLongestPalindromeLen(): Int = {
        @scala.annotation.tailrec
        def calculate(currLen: Int): Int = {
          if(charFreqQueue.isEmpty) {
            currLen
          } else {
            val (_, freq) = charFreqQueue.dequeue()
            if(currLen % 2 == 0) {
              calculate(currLen + freq)
            } else {
              if(freq % 2 == 0) {
                calculate(currLen + freq)
              } else if (freq > 1) {
                calculate(currLen + freq - 1)
              } else {
                currLen
              }
            }
          }
        }
        calculate(0)
      }

      val charFreq = recordCharFreq(s, Map())
      fillQueue(charFreq)
      calculateLongestPalindromeLen()

    }
  }
}
