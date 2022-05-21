package com.algo.dfs

object MaxAreaIsland extends App {
  object Solution {
    def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)
      @scala.annotation.tailrec
      def findMax(row: Int, col: Int, max: Int): Int = {
        if(row == rows) {
          max
        } else if (col == cols) {
          findMax(row + 1, 0, max)
        } else {
          if(grid(row)(col) == 1) {
            val area = dfsFrom(row, col)
            findMax(row, col + 1, Math.max(max, area))
          } else {
            findMax(row, col + 1, max)
          }
        }
      }

      def validCell(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row >= 0 && row < rows && col >= 0 && col < cols
      }

      def findNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row, col + 1), (row, col - 1), (row + 1, col), (row - 1, col)).filter(validCell)
      }

      def dfsFrom(row: Int, col: Int): Int = {
        if(grid(row)(col) != 1) {
          0
        } else {
          grid(row)(col) = -1
          1 + findNeighbours(row, col).map(cell => {
            val (nRow, nCol) = cell
            dfsFrom(nRow, nCol)
          }).sum
        }
      }

      findMax(0, 0, 0)
    }
  }
}
