package com.algo.dfs

object UniquePathsIII extends App {

  val g: Array[Array[Int]] = Array.ofDim[Int](3, 4)
  g(0) = Array(1, 0, 0, 0)
  g(1) = Array(0, 0, 0, 0)
  g(2) = Array(0, 0, 2, -1)

  val res = Solution.uniquePathsIII(g)
  println(res)

  object Solution {

    def uniquePathsIII(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)
      var pathCount = 0

      @scala.annotation.tailrec
      def findStartingPoint(row: Int, col: Int): (Int, Int) = {
        if(row == rows) {
          (-1, -1)
        } else if (col == cols) {
          findStartingPoint(row + 1, 0)
        } else {
          if(isCellStarting((row, col))) {
            (row, col)
          } else {
            findStartingPoint(row, col + 1)
          }
        }
      }

      def countNonObstacleSquares(): Int = grid.map(_.count(_ != -1)).sum

      def isCellValidAndNonObstacle(cell: (Int, Int)): Boolean = {
        isCellValid(cell) && (isCellEmpty(cell) || isCellEnding(cell))
      }

      def isCellValid(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row >= 0 && row < rows && col >= 0 && col < cols
      }

      def isCellEmpty(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        grid(row)(col) == 0
      }

      def isCellEnding(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        grid(row)(col) == 2
      }

      def isCellStarting(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        grid(row)(col) == 1
      }

      def isCellVisited(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        grid(row)(col) == -2
      }

      def findNeighbouringNonObstacleCells(cell: (Int, Int)): Seq[(Int, Int)] = {
        val (row, col) = cell
        Seq(
          (row + 1, col),
          (row - 1, col),
          (row, col + 1),
          (row, col - 1)
        ).filter(isCellValidAndNonObstacle)
      }

      def countUniquePathsFrom(cell: (Int, Int), remaining: Int): Unit = {
        if(isCellEnding(cell) && remaining == 0) {
          pathCount = pathCount + 1
        } else {
          val temp = grid(cell._1)( cell._2)
          grid(cell._1)( cell._2) = -2
          val neighbouringEmptyCells = findNeighbouringNonObstacleCells(cell)
          neighbouringEmptyCells.foreach(neCell => {
            if (!isCellVisited(neCell)) {
              countUniquePathsFrom(neCell, remaining - 1)
            }
          })
          grid(cell._1)( cell._2) = temp
        }
      }

      val (sRow, sCol) = findStartingPoint(0, 0)
      val numOfNonObsCells = countNonObstacleSquares()
      if(sRow == -1 || sCol == -1) 0 else countUniquePathsFrom((sRow, sCol), numOfNonObsCells - 1)
      pathCount
    }
  }
}
