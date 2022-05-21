package com.algo.dp

object NumberOfSubmatricesThatSumToTarget extends App {
  object Solution {
    def numSubmatrixSumTarget(matrix: Array[Array[Int]], target: Int): Int = {
      val rows = matrix.length
      val cols = matrix.headOption.map(_.length).getOrElse(0)

      @scala.annotation.tailrec
      def calculatePrefixSums(row: Int, col: Int): Unit = {
        if(row == rows) {
          ()
        } else if (col == cols) {
          calculatePrefixSums(row + 1, 1)
        } else {
          matrix(row)(col) = matrix(row)(col - 1) + matrix(row)(col)
          calculatePrefixSums(row, col + 1)
        }
      }

      var count = 0

      @scala.annotation.tailrec
      def countSubMatrices(colStart: Int, colEnd: Int): Unit = {
        if(colStart == cols) {
          ()
        } else if (colEnd == cols) {
          countSubMatrices(colStart + 1, colStart + 1)
        } else {
          var map = Map[Int, Int](0 -> 1)
          var sum = 0
          (0 until rows).foreach(row => {
            sum = sum + matrix(row)(colEnd) - (if (colStart > 0) matrix(row)(colStart - 1) else 0)
            count = count + map.getOrElse(sum - target, 0)
            map = map + (sum -> (map.getOrElse(sum, 0) + 1))
          })
          countSubMatrices(colStart, colEnd + 1)
        }
      }

      calculatePrefixSums(0, 1)
      countSubMatrices(0, 0)
      count
    }
  }
}
