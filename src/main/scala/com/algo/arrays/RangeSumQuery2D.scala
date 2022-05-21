package com.algo.arrays

object RangeSumQuery2D extends App {
  class NumMatrix(_matrix: Array[Array[Int]]) {
    val rows: Int = _matrix.length
    val cols: Int = _matrix.headOption.map(_.length).getOrElse(0)
    val rowSums: Array[Array[Int]] = Array.ofDim[Int](rows, cols)

    @scala.annotation.tailrec
    private def fillRowSums(row: Int, col: Int): Unit = {
      if(row == rows) {
        ()
      } else if (col == cols) {
        fillRowSums(row + 1, 0)
      } else {
        if (col == 0) {
          rowSums(row)(col) = _matrix(row)(col)
        } else {
          rowSums(row)(col) = rowSums(row)(col - 1) + _matrix(row)(col)
        }
        fillRowSums(row, col + 1)
      }
    }

    fillRowSums(0, 0)

    def sumRegion(row1: Int, col1: Int, row2: Int, col2: Int): Int = {
      (row1 to row2).map(row => {
        rowSums(row)(col2) - rowSums(row)(col1) + _matrix(row)(col1)
      }).sum
    }
  }

  /**
   * Your NumMatrix object will be instantiated and called as such:
   * var obj = new NumMatrix(matrix)
   * var param_1 = obj.sumRegion(row1,col1,row2,col2)
   */
}
