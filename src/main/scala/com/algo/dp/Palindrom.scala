package com.algo.dp
import scala.collection.immutable

object Palindrom extends App {
  val input = "cbbd"
  Solution.longestPalindrome(input)

  object Solution {
    def longestPalindrome(s: String): String = {
      if(s.length <= 1) s else {
        find(s)
      }
    }

    private def find(string: String): String = {
      val length = string.length
      val matrix = Array.ofDim[Int](length, length)

      @scala.annotation.tailrec
      def fillRow(rowIndex: Int, colIndex: Int, f: String, r: String, maxP: String): (Int, String) = {
        if (colIndex <= length - 1) {
          if (rowIndex == colIndex) {
            matrix(rowIndex)(colIndex) = 1
            fillRow(rowIndex, colIndex + 1, s"${string(rowIndex)}", s"${string(rowIndex)}", s"${string(rowIndex)}")
          } else {
            val fs = s"$f${string(colIndex)}"
            val rs = s"${string(colIndex)}$r"
            if (fs == rs) {
              matrix(rowIndex)(colIndex) = fs.length
              if (fs.length > maxP.length) {
                fillRow(rowIndex, colIndex + 1, fs, rs, fs)
              } else {
                fillRow(rowIndex, colIndex + 1, fs, rs, maxP)
              }
            } else {
              matrix(rowIndex)(colIndex) = 0
              fillRow(rowIndex, colIndex + 1, fs, rs, maxP)
            }
          }
        } else {
//          println(matrix(rowIndex).mkString(","))
//          println(maxP)
          (maxP.length, maxP)
        }
      }

      val longestPLensByRow: immutable.Seq[(Int, String)] = (0 until length).map(row => fillRow(row, row, "", "", ""))
//      println(longestPLensByRow)
      val (_, maxLenP) = longestPLensByRow.max

//      println(maxLen)
//      println(maxLenP)

      maxLenP
    }
  }
}
