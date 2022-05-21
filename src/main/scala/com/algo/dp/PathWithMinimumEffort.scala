package com.algo.dp

object PathWithMinimumEffort extends App {

  object Solution {
    def minimumEffortPath(heights: Array[Array[Int]]): Int = {
      val rows = heights.length
      val cols = heights.headOption.map(_.length).getOrElse(0)
      val visited = Array.ofDim[Boolean](rows, cols)
      val maxEffortFrom = Array.fill[Int](rows, cols)(-1)
      maxEffortFrom(rows - 1)(cols - 1) = 0

      def isCellValid(row: Int, col: Int): Boolean =
        row >= 0 && row < rows && col >= 0 && col < cols

      def calculateEffort(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int): Int =
        Math.abs(heights(fromRow)(fromCol) - heights(toRow)(toCol))

      def unvisitedNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter(cell => {
          val (r, c) = cell
          isCellValid(r, c) && !visited(r)(c)
        })
      }

      def find(row: Int, col: Int): Int = {
        if (row == rows - 1 && col == cols - 1) {
          0
        } else {
          if (maxEffortFrom(row)(col) != -1) {
            maxEffortFrom(row)(col)
          } else {
            visited(row)(col) = true
            val effort =
              unvisitedNeighbours(row, col).map(nCell => {
                val (nr, nc) = nCell
                Math.max(calculateEffort(row, col, nr, nc), find(nr, nc))
              }).min
            maxEffortFrom(row)(col) = effort
            effort
          }
        }
      }

      val o = find(0, 0)
      println(maxEffortFrom.map(_.mkString(",")).mkString("|"))
      o

    }
  }

}
