package com.algo.dp

object MinPathSum extends App {
  val a: Array[Array[Int]] = Array.ofDim[Int](3, 3)
  a(0) = Array(1, 3, 1)
  a(1) = Array(1, 5, 1)
  a(2) = Array(4, 2, 1)
//  val a: Array[Array[Int]] = Array.ofDim[Int](1, 3)
//  a(0) = Array(1, 3, 1)
//  val a: Array[Array[Int]] = Array.ofDim[Int](1, 1)
//  a(0) = Array(1)
  val res = Solution.minPathSum(a)
  println(res)
  object Solution {
    def minPathSum(grid: Array[Array[Int]]): Int = {
      val numRows = grid.length
      val numCols = grid(0).length
      var minDistanceFromCell: Array[Array[Int]] = Array.fill(numRows, numCols)(-1)
      def min(currRow: Int, currCol: Int): Int = {
        if(currRow == numRows - 1 && currCol == numCols - 1) {
          grid(currRow)(currCol)
        } else {
          if(minDistanceFromCell(currRow)(currCol) != -1 ) {
            minDistanceFromCell(currRow)(currCol)
          } else {
            if (currRow == numRows - 1) {
              minDistanceFromCell(currRow)(currCol) = grid(currRow)(currCol) + min(currRow, currCol + 1)
              minDistanceFromCell(currRow)(currCol)
            } else if (currCol == numCols - 1) {
              minDistanceFromCell(currRow)(currCol) = grid(currRow)(currCol) + min(currRow + 1, currCol)
              minDistanceFromCell(currRow)(currCol)
            } else {
              minDistanceFromCell(currRow)(currCol) = grid(currRow)(currCol) + Math.min(
                min(currRow + 1, currCol),
                min(currRow, currCol + 1)
              )
              minDistanceFromCell(currRow)(currCol)
            }
          }
        }
      }
      min(0, 0)
    }
  }
}
