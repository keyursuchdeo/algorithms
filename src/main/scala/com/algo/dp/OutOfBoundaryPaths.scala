package com.algo.dp

object OutOfBoundaryPaths extends App {
  object Solution {
    def findPaths(m: Int, n: Int, maxMove: Int, startRow: Int, startColumn: Int): Int = {
      var map = Map[(Int, Int, Int), Long]()
      val Mod = 1000000007

      def isEdgeRow(row: Int, col: Int): Boolean = {
        row == 0 || row == m - 1 || col == 0 || col == n - 1
      }

      def isCorner(row: Int, col: Int): Boolean = {
        ((row == 0 || row == m - 1) && col == 0) ||
          ((row == 0 || row == m - 1) && col == n - 1)
      }

      def addlOpenSides(row: Int, col: Int, openSides: Int): Int = {
        if((row + 1 == m && row - 1 < 0) && (col + 1 == n && col - 1 < 0)) {
          openSides + 2
        } else if((row + 1 == m && row - 1 < 0) || (col + 1 == n && col - 1 < 0)) {
          openSides + 1
        } else {
          openSides
        }
      }

      def isValid(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row >= 0 && row < m && col >= 0 && col < n
      }

      def findNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter(isValid)
      }

      def countPathsFrom(row: Int, col: Int, remainingMoves: Int): Long = {
        if(remainingMoves <= 0) {
          0
        } else {
          map.get((row, col, remainingMoves)) match {
            case Some(value) =>
              value
            case None =>
              val value =
                if (isCorner(row, col)) {
                  addlOpenSides(row, col, 2) +
                    findNeighbours(row, col).map(neighbour => {
                      val (nRow, nCol) = neighbour
                      countPathsFrom(nRow, nCol, remainingMoves - 1) % Mod
                    }).sum % Mod
                } else if (isEdgeRow(row, col)) {
                  addlOpenSides(row, col, 1) +
                    findNeighbours(row, col).map(neighbour => {
                      val (nRow, nCol) = neighbour
                      countPathsFrom(nRow, nCol, remainingMoves - 1) % Mod
                    }).sum % Mod
                } else {
                  findNeighbours(row, col).map(neighbour => {
                    val (nRow, nCol) = neighbour
                    countPathsFrom(nRow, nCol, remainingMoves - 1) % Mod
                  }).sum % Mod
                }
              map = map + ((row, col, remainingMoves) -> value)
              value
          }
        }
      }

      (countPathsFrom(startRow, startColumn, maxMove) % Mod).toInt
    }
  }
}
