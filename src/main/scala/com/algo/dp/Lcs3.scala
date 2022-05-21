package com.algo.dp

import scala.collection.mutable

object Lcs3 extends App {
  val text1 = "bmvcnwrmxcfcxabkxcvgbozmpspsbenazglyxkpibgzq"
  val text2 = "bmpmlstotylonkvmhqjyxmnqzctonqtobahcrcbibgzgx"
  val res = Solution.longestCommonSubsequence(text1, text2)
  println(res)
  object Solution {
    def longestCommonSubsequence(text1: String, text2: String): Int = {
      var map = mutable.Map[(Int, String), Int]()
      def find(index: Int, remainingText2: String): Int = {
        if(index == text1.length) {
          0
        } else {
          map.get((index, remainingText2)) match {
            case Some(count) => count
            case None =>
              remainingText2.headOption match {
                case Some(head) =>
                  val allIndices = allIndicesOf(head, text1, index)
                  val value = if (allIndices.nonEmpty) {
                    Math.max(
                      1 + allIndices.map(headIndex => {
                        find(headIndex + 1, remainingText2.tail)
                      }).max,
                      find(index, remainingText2.tail)
                    )
                  } else {
                    find(index, remainingText2.tail)
                  }
                  map = map + ((index, remainingText2) -> value)
                  value
                case None =>
                  0
              }
          }
        }
      }

      def allIndicesOf(char: Char, str: String, startIndex: Int): Seq[Int] = {
        str.zipWithIndex.dropWhile(_._2 < startIndex).filter(_._1 == char).map(_._2)
      }

      find(0, text2)
    }
  }
}
