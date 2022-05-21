package com.algo.arrays

object SurroundedRegions2 extends App {

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
      lazy val connectedToBorderZero: Array[Array[Boolean]] = Array.fill[Boolean](rows, cols)(false)
      lazy val connectedToBorderZeroWithIndex: Array[Array[(Boolean, Int)]] = connectedToBorderZero.map(_.zipWithIndex)
      lazy val transposedArray: Array[Array[(Boolean, Int)]] = connectedToBorderZero.transpose.map(_.zipWithIndex)

      def isBorderElement(row: Int, col: Int): Boolean = {
        row == 0 || col == 0 || row == rows - 1 || col == cols - 1
      }

      def isConnectedToBorderElement(row: Int, col: Int): Boolean = {
        connectedToBorderZero(row - 1)(col) ||
          connectedToBorderZero(row + 1)(col) ||
          connectedToBorderZero(row)(col - 1) ||
          connectedToBorderZero(row)(col + 1)
      }

      def fill(row: Int, col: Int): Unit = {
        if(board(row)(col) == 'O' && (isBorderElement(row, col) || isConnectedToBorderElement(row, col))) {
          connectedToBorderZero(row)(col) = true
        } else {
          connectedToBorderZero(row)(col) = false
        }
      }

      @scala.annotation.tailrec
      def fillConnectedToBorderZero(minRow: Int, minCol: Int, maxRow: Int, maxCol: Int): Unit = {
        if(minRow > maxRow || minCol > maxCol) {
          ()
        } else if (minRow == maxRow && minCol == maxCol) {
          if(board(minRow)(minCol) == 'X') {
            ()
          } else {
            fill(minRow, minCol)
          }
        } else if (minRow == maxRow) {
          traverseRow(minRow, minCol, maxCol)
        } else if (minCol == maxCol) {
          traverseCol(minCol, minRow, maxRow)
        } else {
          traverseRow(minRow, minCol, maxCol)
          traverseCol(maxCol, minRow + 1, maxRow)
          traverseRow(maxRow, maxCol - 1, minCol)
          traverseCol(minCol, maxRow - 1, minRow + 1)
          traverseRow(minRow, minCol, maxCol)
          traverseCol(maxCol, minRow + 1, maxRow)
          traverseRow(maxRow, maxCol - 1, minCol)
          traverseCol(minCol, maxRow - 1, minRow + 1)
          fillConnectedToBorderZero(minRow + 1, minCol + 1, maxRow - 1, maxCol - 1)
        }
      }

      def traverseRow(rowIndex: Int, startColIndex: Int, endColIndex: Int): Unit = {
        if (startColIndex < endColIndex) {
          connectedToBorderZeroWithIndex(rowIndex).filter(input => {
            val (_, index) = input
            index >= startColIndex && index <= endColIndex
          }).map(_._2).foreach(col => fill(rowIndex, col))
        } else {
          connectedToBorderZeroWithIndex(rowIndex).filter(input => {
            val (_, index) = input
            index <= startColIndex && index >= endColIndex
          }).map(_._2).foreach(col => fill(rowIndex, col))
        }
      }

      def traverseCol(colIndex: Int, startRowIndex: Int, endRowIndex: Int): Unit = {
        if (startRowIndex < endRowIndex) {
          transposedArray(colIndex).filter(input => {
            val (_, index) = input
            index >= startRowIndex && index <= endRowIndex
          }).map(_._2).foreach(row => fill(row, colIndex))
        } else {
          transposedArray(colIndex).filter(input => {
            val (_, index) = input
            index <= startRowIndex && index >= endRowIndex
          }).map(_._2).foreach(row => fill(row, colIndex))
        }
      }

      @scala.annotation.tailrec
      def find(row: Int = 1, col: Int = 1): Unit = {
        if(row == rows - 1) {
          ()
        } else if (col == cols - 1) {
          find(row + 1, 1)
        } else {
          if(board(row)(col) == 'X' || connectedToBorderZero(row)(col)) {
            find(row, col + 1)
          } else if (board(row)(col) == 'O' && !isConnectedToBorderElement(row, col)){
            board(row)(col) = 'X'
            find(row, col + 1)
          }
//          if(board(row)(col) == 'O' && (!connectedToBorderZero(row)(col) || !isConnectedToBorderElement(row, col))) {
//            board(row)(col) = 'X'
//            find(row, col + 1)
//          } else {
//            find(row, col + 1)
//          }
        }
      }

      if (rows <= 2 || cols <= 2) {
        ()
      } else {
        fillConnectedToBorderZero(0, 0, rows - 1, cols - 1)
        connectedToBorderZero.foreach(z => {
          println(z.mkString(","))
        })
        find(1, 1)
      }
    }

  }
}
