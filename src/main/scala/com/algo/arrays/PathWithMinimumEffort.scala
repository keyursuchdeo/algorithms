package com.algo.arrays

object PathWithMinimumEffort extends App {

  object Solution {
    def minimumEffortPath(heights: Array[Array[Int]]): Int = {
      val rows = heights.length
      val cols = heights.headOption.map(_.length).getOrElse(0)

      def isCellValid(row: Int, col: Int): Boolean =
        row >= 0 && row < rows && col >= 0 && col < cols

      def calculateEffort(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int): Int =
        Math.abs(heights(fromRow)(fromCol) - heights(toRow)(toCol))

      def unvisitedNeighbours(row: Int, col: Int, visited: Array[Array[Boolean]]): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter(cell => {
          val (r, c) = cell
          isCellValid(r, c) && !visited(r)(c)
        })
      }

      def isDestReachableInLimit(row: Int, col: Int, limit: Int, visited: Array[Array[Boolean]]): Boolean = {
        if(row == rows - 1 && col == cols - 1) {
          true
        } else {
          visited(row)(col) = true
          unvisitedNeighbours(row, col, visited).exists(cell => {
            val (nr, nc) = cell
            if(calculateEffort(row, col, nr, nc) <= limit) {
              isDestReachableInLimit(nr, nc, limit, visited)
            } else {
              false
            }
          })
        }
      }

      @scala.annotation.tailrec
      def findMinEffort(low: Int, high: Int, ans: Int): Int = {
        if (high < low) {
          ans
        } else {
          val mid = (high + low) / 2
          val visited = Array.ofDim[Boolean](rows, cols)
          if(isDestReachableInLimit(0, 0, mid, visited)) {
            findMinEffort(low, mid - 1, mid)
          } else {
            findMinEffort(mid + 1, high, ans)
          }
        }
      }

      findMinEffort(0, Int.MaxValue, Int.MaxValue)

    }
  }

}
