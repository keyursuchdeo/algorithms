package com.algo.arrays

object SortMatrixDiagonally extends App {
  object Solution {
    def diagonalSort(mat: Array[Array[Int]]): Array[Array[Int]] = {
      val rows = mat.length
      val cols = mat.headOption.map(_.length).getOrElse(0)

      @scala.annotation.tailrec
      def gather(row: Int, col: Int, elements: Seq[Int] = Nil): Seq[Int] = {
        if(row == rows || col == cols) {
          elements
        } else {
          gather(row + 1, col + 1, mat(row)(col) +: elements)
        }
      }

      @scala.annotation.tailrec
      def putSortedElements(row: Int, col: Int, elements: Seq[Int]): Unit = {
        if(row == rows || col == cols) {
          ()
        } else {
          mat(row)(col) = elements.head
          putSortedElements(row + 1, col + 1, elements.tail)
        }
      }

      @scala.annotation.tailrec
      def traverseAndSortRow0Diagonals(row: Int): Unit= {
        if(row < 0) {
          ()
        } else {
          val elements = gather(row, 0)
          putSortedElements(row, 0, elements.sorted)
          traverseAndSortRow0Diagonals(rows - 1)
        }
      }

      @scala.annotation.tailrec
      def traverseAndSortCol0Diagonals(col: Int): Unit= {
        if(col == cols) {
          ()
        } else {
          val elements = gather(0, col)
          putSortedElements(0, col, elements.sorted)
          traverseAndSortCol0Diagonals(col + 1)
        }
      }

      traverseAndSortRow0Diagonals(rows - 1)
      traverseAndSortCol0Diagonals(1)
      mat
    }
  }
}
