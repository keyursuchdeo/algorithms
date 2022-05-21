package com.algo.dp

import scala.util.Try

object LongestIncreasingPathMatrix extends App {

  object Solution {
    def longestIncreasingPath(matrix: Array[Array[Int]]): Int = {
      val rows = matrix.length
      val cols = matrix.headOption.map(_.length).getOrElse(0)
      val longestPaths = Array.ofDim[Int](rows, cols)

      def validCell(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row >= 0 && row < rows && col >= 0 && col < cols
      }

      def findNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter(validCell)
      }

      def dfsFrom(row: Int, col: Int): Int = {
        if (row == rows) {
          0
        } else if (col == cols) {
          dfsFrom(row + 1, 0)
        } else {
          if (longestPaths(row)(col) != 0) {
            longestPaths(row)(col)
          } else {
            val max =
              Try(findNeighbours(row, col).collect {
                case (nr, nc) if matrix(nr)(nc) > matrix(row)(col) =>
                  dfsFrom(nr, nc)
              }.max).toOption.getOrElse(0)
            longestPaths(row)(col) = 1 + max
            longestPaths(row)(col)
          }
        }
      }

      @scala.annotation.tailrec
      def fillLongestPath(row: Int, col: Int): Unit = {
        if (row == rows) {
          ()
        } else if (col == cols) {
          fillLongestPath(row + 1, 0)
        } else {
          longestPaths(row)(col) = dfsFrom(row, col)
          fillLongestPath(row, col + 1)
        }
      }

      dfsFrom(0, 0)
      println(longestPaths.map(_.mkString(",")).mkString("|"))
      longestPaths.map(_.max).max
    }
  }

}
