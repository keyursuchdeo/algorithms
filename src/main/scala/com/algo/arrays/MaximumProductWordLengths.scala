package com.algo.arrays

object MaximumProductWordLengths extends App {
  object Solution {
    def maxProduct(words: Array[String]): Int = {
      val charSets = new Array[Set[Char]](words.length)

      @scala.annotation.tailrec
      def fillCharSets(index: Int): Unit = {
        if(index == words.length) {
          ()
        } else {
          charSets(index) = words(index).toSet
          fillCharSets(index + 1)
        }
      }

      @scala.annotation.tailrec
      def calculateFrom(index: Int, max: Int, charSet: Set[Char], len: Int): Int = {
        if(index == words.length) {
          max
        } else {
          val charSetAtIndex = charSets(index)
          if (charSet -- charSetAtIndex == charSet) {
            calculateFrom(index + 1, Math.max(max, len * words(index).length), charSet, len)
          } else {
            calculateFrom(index + 1, max, charSet, len)
          }
        }
      }

      @scala.annotation.tailrec
      def calculate(index : Int, max: Int): Int = {
        if(index == words.length) {
          max
        } else {
          val maxAtIndex = calculateFrom(index + 1, 0, charSets(index), words(index).length)
          calculate(index + 1, Math.max(max, maxAtIndex))
        }
      }
      fillCharSets(0)
      calculate(0, 0)

    }
  }
}
