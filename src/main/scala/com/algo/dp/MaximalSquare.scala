package com.algo.dp

object MaximalSquare extends App {

    val array = Array.ofDim[Char](4, 5)
    array(0) = Array('1', '0', '1', '0', '0')
    array(1) = Array('1', '0', '1', '1', '1')
    array(2) = Array('1', '1', '1', '1', '1')
    array(3) = Array('1', '0', '0', '1', '0')

//  val array = Array.ofDim[Char](6, 6)
//  array(0) = Array('1', '0', '1', '1', '0', '1')
//  array(1) = Array('1', '1', '1', '1', '1', '1')
//  array(2) = Array('0', '1', '1', '0', '1', '1')
//  array(3) = Array('1', '1', '1', '0', '1', '0')
//  array(4) = Array('0', '1', '1', '1', '1', '1')
//  array(5) = Array('1', '1', '0', '1', '1', '1')

  Solution.maximalSquare(array)

  object Solution {
    def maximalSquare(matrix: Array[Array[Char]]): Int = {
      if (matrix.length == 0) 0 else {
        val rows = matrix.length
        val cols = matrix(0).length
        val squareSize = Array.ofDim[Int](rows, cols)

        @scala.annotation.tailrec
        def find(rowIndex: Int, colIndex: Int): Unit = {
          if (colIndex == cols) {
            find(rowIndex + 1, 0)
          } else if (rowIndex == rows) {
            ()
          } else {
            if (rowIndex == 0 || colIndex == 0) {
              squareSize(rowIndex)(colIndex) = if (matrix(rowIndex)(colIndex) == '1') 1 else 0
              find(rowIndex, colIndex + 1)
            } else {
              if (matrix(rowIndex)(colIndex) == '1') {
                squareSize(rowIndex)(colIndex) =
                  Math.min(
                    Math.min(
                      squareSize(rowIndex - 1)(colIndex), squareSize(rowIndex - 1)(colIndex - 1)
                    ),
                    squareSize(rowIndex)(colIndex - 1)) + 1
              }
              find(rowIndex, colIndex + 1)
            }
          }
        }

        find(0, 0)
        val a: String = squareSize.map(_.mkString(",")).mkString("|")
        println(a)
        val maxSize = squareSize.flatten.max
        println(maxSize)
        maxSize
      }
    }
  }

}
