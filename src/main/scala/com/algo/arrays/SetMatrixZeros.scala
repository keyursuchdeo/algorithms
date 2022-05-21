package com.algo.arrays

object SetMatrixZeros extends App {
  object Solution {
    def setZeroes(matrix: Array[Array[Int]]): Unit = {
      val rows = matrix.length
      val cols = matrix.headOption.map(_.length).getOrElse(0)
      case class Cell(row: Int, col: Int)

      @scala.annotation.tailrec
      def findZeros(row: Int, col: Int, cells: Seq[Cell]): Seq[Cell] = {
        if(row == rows) {
          cells
        } else if (col == cols) {
          findZeros(row + 1, 0, cells)
        } else {
          if(matrix(row)(col) == 0) {
            findZeros(row, col + 1, Cell(row, col) +: cells)
          } else {
            findZeros(row, col + 1, cells)
          }
        }
      }

      @scala.annotation.tailrec
      def makeZeros(zeroCells: Seq[Cell]): Unit = {
        if(zeroCells.isEmpty) {
          ()
        } else {
          makeRowZero(zeroCells.head.row)
          makeColZero(zeroCells.head.col)
          makeZeros(zeroCells.tail)
        }
      }

      def makeRowZero(row: Int): Unit = {
        matrix(row) = matrix(row).map(_ => 0)
      }

      def makeColZero(col: Int): Unit = {
        @scala.annotation.tailrec
        def makeZero(row: Int): Unit = {
          if(row == rows) {
            ()
          } else {
            matrix(row)(col) = 0
            makeZero(row + 1)
          }
        }
        makeZero(0)
      }

      makeZeros(findZeros(0, 0, Nil))
    }
  }
}
