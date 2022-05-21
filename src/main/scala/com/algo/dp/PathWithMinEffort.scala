package com.algo.dp

object PathWithMinEffort extends App {
  object Solution {
    def minimumEffortPath(heights: Array[Array[Int]]): Int = {
      val rows: Int = heights.length
      val cols: Int = heights.headOption.map(_.length).getOrElse(0)
      val efforts = Array.fill[Int](rows, cols)(-1)
      val visited = Array.fill[Int](rows, cols)(-1)



      def calculateEffort(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int) = {
        if(invalidCell(toRow, toCol)) {
          Int.MaxValue
        } else {
          Math.abs(heights(fromRow)(fromCol) - heights(toRow)(toCol))
        }
      }

      def invalidCell(row: Int, col: Int) = row < 0 || row >= rows || col < 0 || col >= cols

      def min(num1: Int, num2: Int, num3: Int, num4: Int): Int =
        Math.min(num1, Math.min(num2, Math.min(num3, num4)))

      def findPathWithMinEffort(row: Int, col: Int, prevMaxEffortInPath: Int): Int = {
        if(row == rows - 1 && col == cols - 1) {
          prevMaxEffortInPath
        } else if (invalidCell(row, col)) {
          Int.MaxValue
        } else if (efforts(row)(col) != -1) {
          efforts(row)(col)
        } else if (visited(row)(col) != -1) {
          Int.MaxValue
        } else {
          visited(row)(col) = 1
          efforts(row)(col) =
          min(
            findPathWithMinEffort(row, col + 1, Math.max(prevMaxEffortInPath, calculateEffort(row, col, row, col + 1))),
              findPathWithMinEffort(row, col - 1, Math.max(prevMaxEffortInPath, calculateEffort(row, col, row, col - 1))),
              findPathWithMinEffort(row + 1, col, Math.max(prevMaxEffortInPath, calculateEffort(row, col, row + 1, col))),
              findPathWithMinEffort(row - 1, col, Math.max(prevMaxEffortInPath, calculateEffort(row, col, row - 1, col))),
          )
          efforts(row)(col)
        }
      }

      findPathWithMinEffort(0, 0, Int.MinValue)
      efforts.head.head
    }
  }
}
