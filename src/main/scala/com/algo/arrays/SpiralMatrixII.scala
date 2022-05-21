package com.algo.arrays

object SpiralMatrixII extends App {

  Solution.generateMatrix(2)

  object Solution {
    def generateMatrix(n: Int): Array[Array[Int]] = {
      val elemCount: Int = n * n
      val output = Array.ofDim[Int](n, n)

      def plus(num: Int) = num + 1

      def minus(num: Int) = num - 1

      @scala.annotation.tailrec
      def fillCol(row: Int, col: Int, rowLimit: Int, currN: Int, op: Int => Int): Int = {
        if (row == rowLimit) {
          currN
        } else {
          output(row)(col) = currN
          fillCol(op(row), col, rowLimit, currN + 1, op)
        }
      }

      @scala.annotation.tailrec
      def fillRow(row: Int, col: Int, colLimit: Int, currN: Int, op: Int => Int): Int = {
        if (col == colLimit) {
          currN
        } else {
          output(row)(col) = currN
          fillRow(row, op(col), colLimit, currN + 1, op)
        }
      }

      @scala.annotation.tailrec
      def generate(row: Int, col: Int, uRowLimit: Int, lRowLimit: Int, uColLimit: Int, lColLimit: Int, currN: Int): Unit = {
        if (currN > elemCount) {
          ()
        } else {
          val n1 = fillRow(row, col, uColLimit, currN, plus)
          val n2 = if (n1 <= elemCount) fillCol(row + 1, uColLimit - 1, uRowLimit, n1, plus) else n1
          val n3 = if (n2 <= elemCount) fillRow(uRowLimit - 1, uColLimit - 2, lColLimit, n2, minus) else n2
          val n4 = if (n3 <= elemCount) fillCol(uRowLimit - 2, lColLimit + 1, lRowLimit + 1, n3, minus) else n3
          generate(row + 1, col + 1, uRowLimit - 1, lRowLimit + 1, uColLimit - 1, lColLimit + 1, n4)
        }
      }

      generate(0, 0, n, -1, n, -1, 1)
      output
    }
  }

}
