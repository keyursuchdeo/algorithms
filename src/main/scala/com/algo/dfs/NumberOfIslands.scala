package com.algo.dfs

object NumberOfIslands extends App {
  object Solution {
    def numIslands(grid: Array[Array[Char]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)
      val visitedIslands = Array.fill[Boolean](rows, cols)(false)

      @scala.annotation.tailrec
      def countIslands(row: Int, col: Int, count: Int): Int = {
        if(row == rows) {
          count
        } else if (col == cols) {
          countIslands(row + 1, 0, count)
        } else {
          if(isInIsland(row, col) && !isVisited(row, col)) {
            bfs(row, col)
            countIslands(row, col + 1, count + 1)
          } else {
            countIslands(row, col + 1, count)
          }
        }
      }

      def bfs(row: Int, col: Int): Unit = {
        visitedIslands(row)(col) = true
        neighbouringCells(row, col).foreach(cell => {
          val (row, col) = cell
          if(validCell(row, col) && isInIsland(row, col) && !isVisited(row, col)) {
            bfs(row, col)
          } else {
            ()
          }
        })
      }

      def isInIsland(row: Int, col: Int) = {
        grid(row)(col) == '1'
      }

      def isVisited(row: Int, col: Int) = {
        visitedIslands(row)(col)
      }

      def neighbouringCells(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1))
      }

      def validCell(row: Int, col: Int): Boolean = {
        row >= 0 && row < rows && col >= 0 && col < cols
      }

      countIslands(0, 0, 0)
    }
  }
}
