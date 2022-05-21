package com.algo.backtracking

object SudokuSolver extends App {
  object Solution {
    def solveSudoku(board: Array[Array[Char]]): Unit = {
      val rows = board.length
      val cols = board.headOption.map(_.length).getOrElse(0)
      val masterSet = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val rowMap = new Array[Set[Int]](rows)
      val colMap = new Array[Set[Int]](cols)
      val gridMap = Array.ofDim[Set[Int]](rows/3, cols/3)

      @scala.annotation.tailrec
      def fillMaps(row: Int, col: Int): Unit = {
        if(row == rows && col == cols) {
          ()
        } else if (col == cols) {
          fillMaps(row + 1, 0)
        } else {
          if(board(row)(col) == '.') {
            fillMaps(row, col + 1)
          } else {
            val num = board(row)(col).asDigit
            rowMap(row) = rowMap(row) + num
            colMap(col) = colMap(col) + num
            gridMap(row/3)(col/3) = gridMap(row/3)(col/3) + num
            fillMaps(row, col + 1)
          }
        }
      }

      def findPossibleValues(row: Int, col: Int): Set[Int] = {
        masterSet -- rowMap(row) -- colMap(col) -- gridMap(row/3)(col/3)
      }

//      def solveGrid(row: Int, col: Int) = {
//        if(row == )
//      }
    }
  }
}
