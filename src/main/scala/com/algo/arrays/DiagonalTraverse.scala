package com.algo.arrays

object DiagonalTraverse extends App {

  val a = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
  val res = Solution.findDiagonalOrder(a)
  println(res.mkString(","))

  object Solution {
    def findDiagonalOrder(matrix: Array[Array[Int]]): Array[Int] = {
      val rows = matrix.length
      val cols = matrix.headOption.map(_.length).getOrElse(0)
      val maxSum = (rows - 1) + (cols - 1)

      def validCell(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row >= 0 && row < rows && col >= 0 && col < cols
      }

      def getMatrixElements(sum: Int, fromTop: Boolean): Seq[Int] = {
        if(fromTop) {
          (0 to sum).collect {
            case index: Int if validCell((index, sum - index)) => matrix(index)(sum - index)
          }
        } else {
          (0 to sum).collect {
            case index: Int if validCell((sum - index, index)) => matrix(sum - index)(index)
          }
        }
      }

      def build(index: Int, fromTop: Boolean): Seq[Int] = {
        if(index == maxSum) {
          getMatrixElements(index, fromTop)
        } else if (index == 0) {
          Seq(matrix(index)(index)) ++ build(index + 1, fromTop = true)
        } else {
          getMatrixElements(index, fromTop) ++ build(index + 1, !fromTop)
        }
      }

      if(matrix.isEmpty) Array() else build(0, fromTop = true).toArray
    }
  }
}
