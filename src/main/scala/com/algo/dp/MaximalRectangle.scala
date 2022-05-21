package com.algo.dp

object MaximalRectangle extends App {

  val m = Array.ofDim[Char](2, 2)
  m(0) = Array('1', '0')
  m(1) = Array('0', '1')
//  val res = Solution.maximalRectangle(m)
//  println(res)

  object Solution {
//    def maximalRectangle(matrix: Array[Array[Char]]): Int = {
//      val numOfCols = matrix(0).length
//      val numOfRows = matrix.length
//      val sums = Array.ofDim[Int](numOfRows, numOfCols)
//      def calcMaxArea(): Int = {
//        for(
//          i <- 0 until numOfRows;
//          j <- 0 until numOfCols
//        ) {
//          if (i == 0) {
//            sums(i)(j) = if (matrix(i)(j) == '0') 0 else 1
//          } else {
//            sums(i)(j) = if(matrix(i)(j) == '0') 0 else sums(i - 1)(j) + 1
//          }
//        }
//      }
//
//      calcMaxArea(0, 0)
//    }
  }
}
