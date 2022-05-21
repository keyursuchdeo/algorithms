package com.algo.dp

object SquareSubMatrices extends App {

//  val input = Array.ofDim[Int](3, 3)
//  input(0) = Array(1, 0, 1)
//  input(1) = Array(1, 1, 0)
//  input(2) = Array(1, 1, 0)

//  val input = Array.ofDim[Int](3, 4)
//  input(0) = Array(0, 1, 1, 1)
//  input(1) = Array(1, 1, 1, 1)
//  input(2) = Array(0, 1, 1, 1)

  val input = Array.ofDim[Int](1, 1)
  input(0) = Array(1)

  val res = Solution.countSquares(input)
  println(res)

  object Solution {
    def countSquares(matrix: Array[Array[Int]]): Int = {
      val rows = matrix.length
      val cols = matrix(0).length
      val squares = Array.ofDim[Int](rows, cols)

      @scala.annotation.tailrec
      def count(row: Int, col: Int): Unit = {
        if (row == rows) {
          ()
        } else if (row == 0 || col == 0) {
          squares(row)(col) = matrix(row)(col)
          if (col < cols - 1) {
            count(row, col + 1)
          } else {
            count(row + 1, 0)
          }
        } else {
          if (matrix(row)(col) == 0) {
            squares(row)(col) = 0
          } else {
            squares(row)(col) = 1 +
              Math.min(
                Math.min(
                  squares(row - 1)(col),
                  squares(row)(col - 1)
                ),
                squares(row - 1)(col - 1)
              )
          }
          if (col < cols - 1) {
            count(row, col + 1)
          } else {
            count(row + 1, 0)
          }
        }
      }

      count(0, 0)
      println(squares.map(_.mkString(",")).mkString("|"))
      squares.map(_.sum).sum
    }
  }

}
