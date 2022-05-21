package com.algo.arrays

object SpiralMatrix2 extends App {

//  val a = Array.ofDim[Int](4, 4)
//  a(0) = Array(1, 2, 3, 4)
//  a(1) = Array(5, 6, 7, 8)
//  a(2) = Array(9, 10, 11, 12)
//  a(3) = Array(13, 14, 15, 16)

  val a = Array.ofDim[Int](2, 1)
  a(0) = Array(1)
  a(1) = Array(2)

  val res = Solution.spiralOrder(a)
  println(res)

  object Solution {
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {

      lazy val matrixWithIndex: Array[Array[(Int, Int)]] = matrix.map(_.zipWithIndex)
      lazy val transposedArray: Array[Array[(Int, Int)]] = matrix.transpose.map(_.zipWithIndex)
      lazy val rows = matrix.length
      lazy val cols = matrix.headOption.map(_.length).getOrElse(0)

      @scala.annotation.tailrec
      def traverse(minRow: Int, minCol: Int, maxRow: Int, maxCol: Int, list: List[Int]): List[Int] = {
        if(minRow > maxRow || minCol > maxCol) {
          list
        } else if (minRow == maxRow && minCol == maxCol) {
          list :+ matrix(minRow)(minCol)
        } else if (minRow == maxRow) {
          list ++ traverseRow(minRow, minCol, maxCol).toList
        } else if (minCol == maxCol) {
          list ++ traverseCol(minCol, minRow, maxRow).toList
        } else {
          val a = traverseRow(minRow, minCol, maxCol).toList
          val b = traverseCol(maxCol, minRow + 1, maxRow).toList
          val c = traverseRow(maxRow, maxCol - 1, minCol).toList
          val d =
            if(maxRow - 1 >= minRow + 1) {
              traverseCol(minCol, maxRow - 1, minRow + 1).toList
            } else {
              Nil
            }
          val spiralOrder = a ++ b ++ c ++ d
          traverse(minRow + 1, minCol + 1, maxRow - 1, maxCol - 1, list ++ spiralOrder)
        }
      }

      def traverseRow(rowIndex: Int, startColIndex: Int, endColIndex: Int): Array[Int] = {
        if (startColIndex < endColIndex) {
          matrixWithIndex(rowIndex).filter(input => {
            val (_, index) = input
            index >= startColIndex && index <= endColIndex
          }).map(_._1)
        } else {
          matrixWithIndex(rowIndex).filter(input => {
            val (_, index) = input
            index <= startColIndex && index >= endColIndex
          }).map(_._1).reverse
        }
      }

      def traverseCol(colIndex: Int, startRowIndex: Int, endRowIndex: Int) = {
        if (startRowIndex < endRowIndex) {
          transposedArray(colIndex).filter(input => {
            val (_, index) = input
            index >= startRowIndex && index <= endRowIndex
          }).map(_._1)
        } else {
          transposedArray(colIndex).filter(input => {
            val (_, index) = input
            index <= startRowIndex && index >= endRowIndex
          }).map(_._1).reverse
        }
      }

      traverse(0, 0, rows - 1, cols - 1, Nil)
    }
  }

}
