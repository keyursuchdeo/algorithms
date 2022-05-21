package com.algo.stack

import scala.util.Try

object NextGreaterElementIII extends App {
  object Solution {
    def nextGreaterElement(n: Int): Int = {
      val nChars = n.toString.toCharArray
      @scala.annotation.tailrec
      def findDigitIndex(index: Int): Int = {
        if(index < 0) {
          -1
        } else {
          if(index == nChars.length - 1) {
            findDigitIndex(index - 1)
          } else {
            if(nChars(index) >= nChars(index + 1)) {
              findDigitIndex(index - 1)
            } else {
              index
            }
          }
        }
      }

      def findSmallestDigitGreaterThanFrom(fromIndex: Int): Int = {
        @scala.annotation.tailrec
        def find(index: Int, outputIndex: Int): Int = {
          if(index == nChars.length) {
            outputIndex
          } else {
            if(nChars(index) > nChars(fromIndex)) {
              find(index + 1, index)
            } else {
              outputIndex
            }
          }
        }
        find(fromIndex + 1, fromIndex)
      }

      if(n == Int.MaxValue || n < 12) {
        -1
      } else {
        val digitIndex = findDigitIndex(nChars.length - 1)

        if(digitIndex == -1) {
          -1
        } else {
          val swapWith: Int =
            if(digitIndex == nChars.length - 1) digitIndex -1 else findSmallestDigitGreaterThanFrom(digitIndex)
          val temp = nChars(digitIndex)
          nChars(digitIndex) = nChars(swapWith)
          nChars(swapWith) = temp
          val (before, after) = nChars.splitAt(digitIndex + 1)
          Try((before ++ after.reverse).mkString("").toInt).getOrElse(-1)
        }
      }
    }
  }
}
