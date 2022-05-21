package com.algo.arrays

object MaximumAreaCakeAfterHorizontalVerticalCuts extends App {
  object Solution {
    def maxArea(h: Int, w: Int, horizontalCuts: Array[Int], verticalCuts: Array[Int]): Int = {
      val sortedHorizontalCuts = horizontalCuts.sorted
      val sortedVerticalCuts = verticalCuts.sorted
      val Mod = 1000000007

      @scala.annotation.tailrec
      def findMaxHeight(index: Int = 1, maxHeight: Int): Int = {
        if(index == horizontalCuts.length) {
          Math.max(maxHeight, h - sortedHorizontalCuts(index - 1))
        } else {
          val currPieceHeight = sortedHorizontalCuts(index) - sortedHorizontalCuts(index - 1)
          findMaxHeight(index + 1, Math.max(maxHeight, currPieceHeight))
        }
      }

      @scala.annotation.tailrec
      def findMaxWidth(index: Int = 1, maxWidth: Int): Int = {
        if(index == verticalCuts.length) {
          Math.max(maxWidth, w - sortedVerticalCuts(index - 1))
        } else {
          val currPieceWidth = sortedVerticalCuts(index) - sortedVerticalCuts(index - 1)
          findMaxWidth(index + 1, Math.max(maxWidth, currPieceWidth))
        }
      }

      val height = findMaxHeight(maxHeight = sortedHorizontalCuts(0))
      val width = findMaxWidth(maxWidth = sortedVerticalCuts(0))

      println(height)
      println(width)
      (height * width % Mod)
    }
  }
}
