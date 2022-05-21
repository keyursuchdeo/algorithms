package com.algo.arrays

object ShortestDistanceToCharacter extends App {
  object Solution {
    def shortestToChar(s: String, c: Char): Array[Int] = {
      val chars = s.toCharArray
      val shortestDist = new Array[Int](chars.length)

      @scala.annotation.tailrec
      def findIndicesOfChar(index: Int, indices: Seq[Int]): Array[Int] = {
        if(index < 0) {
          indices.toArray
        } else {
          if(chars(index) == c) {
            findIndicesOfChar(index - 1, index +: indices)
          } else {
            findIndicesOfChar(index - 1, indices)
          }
        }
      }

      val indicesOfC = findIndicesOfChar(chars.length - 1, Nil)

      def findIndexClosestTo(index: Int): Int = {
        @scala.annotation.tailrec
        def find(low: Int, high: Int, closestIndex: Int): Int = {
          if(high < low) {
            indicesOfC(closestIndex)
          } else {
            val mid = (low + high) / 2
            val distFromMid = Math.abs(index - indicesOfC(mid))
            val distFromClosestIndex = Math.abs(index - indicesOfC(closestIndex))
            val updatedClosestIndex =
              if(distFromMid < distFromClosestIndex) mid else closestIndex
            if(index < indicesOfC(mid)) {
              find(low, mid - 1, updatedClosestIndex)
            } else {
              find(mid + 1, high, updatedClosestIndex)
            }
          }
        }
        find(0, indicesOfC.length - 1, 0)
      }

      @scala.annotation.tailrec
      def fillShortestDistance(index: Int): Unit = {
        if(index == chars.length - 1) {
          ()
        } else {
          if(chars(index) == c) {
            shortestDist(index) = 0
            fillShortestDistance(index + 1)
          } else {
            shortestDist(index) = Math.abs(index - findIndexClosestTo(index))
            fillShortestDistance(index + 1)
          }
        }
      }

      println(indicesOfC.mkString(","))
      fillShortestDistance(0)
      shortestDist
    }
  }
}
