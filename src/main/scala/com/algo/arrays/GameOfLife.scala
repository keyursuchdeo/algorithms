package com.algo.arrays

object GameOfLife extends App {

  object Solution {
    def gameOfLife(board: Array[Array[Int]]): Unit = {
      val rows = board.length
      val cols = board.headOption.map(_.length).getOrElse(0)

      def isValidCell(row: Int, col: Int): Boolean = {
        row >= 0 && col >= 0 && row < rows && col < cols
      }

      def getNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq(
          (row, col + 1),
          (row + 1, col),
          (row, col - 1),
          (row - 1, col),
          (row - 1, col + 1),
          (row + 1, col + 1),
          (row + 1, col - 1),
          (row - 1, col - 1)
        ).filter(cell => {
          val (r, c) = cell
          isValidCell(r, c)
        })
      }

      def getNextState(row: Int, col: Int, prevRow: Array[Int], nextRow: Array[Int]) = {
        val neighbours: Seq[(Int, Int)] = getNeighbours(row, col)
        val numOfLiveNeighbours: Int = neighbours.count(cell => {
          val (r, c) = cell
          if(r == row) {
            board(r)(c) == 1
          } else if (r == row - 1) {
            prevRow(c) == 1
          } else {
            nextRow(c) == 1
          }
        })

        if(board(row)(col) == 0) {
          if(numOfLiveNeighbours == 3) 1 else 0
        } else {
          if(numOfLiveNeighbours < 2 || numOfLiveNeighbours > 3) 0 else 1
        }
      }

      @scala.annotation.tailrec
      def generateNextState(row: Int, col: Int, nextStateCurrRow: Array[Int], prevRow: Array[Int], nextRow: Array[Int]): Unit = {
        if (row == rows) {
          ()
        } else if (col == cols) {
          val currRow = board(row)
          val newNextRow = if(row + 2 < rows) board(row + 2) else Array.empty[Int]
          board(row) = nextStateCurrRow
          generateNextState(row + 1, 0, new Array[Int](cols), currRow, newNextRow)
        } else {
          nextStateCurrRow(col) = getNextState(row, col, prevRow, nextRow)
          generateNextState(row, col + 1, nextStateCurrRow, prevRow, nextRow)
        }
      }

      val newNextRow = if(1 < rows) board(1) else Array.empty[Int]
      generateNextState(0, 0, new Array[Int](cols), Array.empty[Int], newNextRow)
    }
  }

}
