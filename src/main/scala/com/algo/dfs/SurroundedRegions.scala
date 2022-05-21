package com.algo.dfs

object SurroundedRegions extends App {

  val b = Array.ofDim[Char](6, 6)
  b(0) = Array('O','X','O','O','O','X')
  b(1) = Array('O','O','X','X','X','O')
  b(2) = Array('X','X','X','X','X','O')
  b(3) = Array('O','O','O','O','X','X')
  b(4) = Array('X','X','O','O','X','O')
  b(5) = Array('O','O','X','X','X','X')

  Solution.solve(b)
  b.foreach(row => {
    println(row.mkString(","))
  })

  object Solution {
    def solve(board: Array[Array[Char]]): Unit = {
      val rows = board.length
      val cols = board.headOption.map(_.length).getOrElse(0)

      @scala.annotation.tailrec
      def traverseRow(row: Int): Unit = {
        if(row == rows) {
          ()
        } else {
          if(board(row)(0) == 'O') {
            dfs(row, 0)
          }
          if(board(row)(cols - 1) == 'O') {
            dfs(row, cols - 1)
          }
          traverseRow(row + 1)
        }
      }

      @scala.annotation.tailrec
      def traverseCol(col: Int): Unit = {
        if(col == cols) {
          ()
        } else {
          if(board(0)(col) == 'O') {
            dfs(0, col)
          }
          if(board(rows - 1)(col) == 'O') {
            dfs(rows - 1, col)
          }
          traverseCol(col + 1)
        }
      }

      @scala.annotation.tailrec
      def fill(row: Int, col: Int): Unit = {
        if (row == rows) {
          ()
        } else if (col == cols) {
          fill (row + 1, 0)
        } else {
          if(board(row)(col) == 'O') {
            board(row)(col) = 'X'
          } else if (board(row)(col) == 'P') {
            board(row)(col) = 'O'
          } else {
            ()
          }
          fill(row, col + 1)
        }
      }


      def dfs(row: Int, col: Int): Unit = {
        if(row >= 0 && row < board.length && col >= 0 && col < board(0).length && board(row)(col) == 'O') {
          board(row)(col) = 'P';
          dfs(row + 1, col);
          dfs(row, col + 1);
          dfs(row - 1, col);
          dfs(row, col - 1);
        } else {
          ()
        }
      }

      traverseRow(0)
      traverseCol(0)
      fill(0, 0)

    }

  }
}
