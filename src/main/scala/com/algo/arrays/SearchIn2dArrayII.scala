package com.algo.arrays

object SearchIn2dArrayII extends App {

  val m = Array.ofDim[Int](7, 6)
  m(0) = Array(3, 3, 8, 13, 13, 18)
  m(1) = Array(4, 5, 11, 13, 18, 20)
  m(2) = Array(9, 9, 14, 15, 23, 23)
  m(3) = Array(13, 18, 22, 22, 25, 27)
  m(4) = Array(18, 22, 23, 28, 30, 33)
  m(5) = Array(21, 25, 28, 30, 35, 35)
  m(6) = Array(24, 25, 33, 36, 37, 40)

  val res = Solution.searchMatrix(m, 21)
  println(res)


  object Solution {
    def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
      val rows = matrix.length
      val cols = matrix.headOption.map(_.length).getOrElse(0)

      @scala.annotation.tailrec
      def searchColsOfRow(row: Int, low: Int, high: Int): Boolean = {
        if (high < low) {
          false
        } else {
          val mid = (low + high) / 2
          if (target > matrix(row)(mid)) {
            (low to mid).exists(index => {
              searchRowsOfCol(index, row, rows - 1)
            }) || searchColsOfRow(row, mid + 1, high)
          } else if (target < matrix(row)(mid)) {
            searchColsOfRow(row, low, mid - 1)
          } else {
            true
          }
        }
      }

      @scala.annotation.tailrec
      def searchRowsOfCol(col: Int, low: Int, high: Int): Boolean = {
        if (high < low) {
          false
        } else {
          val mid = (low + high) / 2
          if (matrix(mid)(col) > target) {
            searchRowsOfCol(col, low, mid - 1)
          } else if (matrix(mid)(col) < target) {
            searchRowsOfCol(col, mid + 1, high)
          } else {
            true
          }
        }
      }

      searchColsOfRow(0, 0, cols - 1)
    }
  }

}
