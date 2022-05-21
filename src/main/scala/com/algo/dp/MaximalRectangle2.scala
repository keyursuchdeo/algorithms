package com.algo.dp

object MaximalRectangle2 extends App {
//  object Solution {
//    def maximalRectangle(matrix: Array[Array[Char]]): Int = {
//      val rows = matrix.length
//      val cols = matrix.headOption.map(_.length).getOrElse(0)
//      val visited = Array.ofDim[Boolean](rows, cols)
//      val area = Array.ofDim[Int](rows, cols)
//
//      @scala.annotation.tailrec
//      def findOne(row: Int, col: Int): Unit = {
//        if(row == rows) {
//          ()
//        } else if (col == cols) {
//          findOne(row + 1, 0)
//        } else {
//          if(matrix(row)(col) == '0') {
//            findOne(row, col + 1)
//          } else {
//            visited(row)(col) = true
//            findOne(row, col + 1)
//          }
//        }
//      }
//
//      def calculateAreaFromCell(row: Int, col: Int): Int = {
//        if(matrix(row)(col + 1) == '1' && matrix(row + 1)(col) == '1' && matrix(row + 1)(col + 1) == '1') {
//          4 + Math.max(
//            calculateAreaFromCell(row, col + 2),
//            calculateAreaFromCell(row + 2, col)
//          )
//        } else if (matrix(row)(col + 1) == '1') {
//
//        } else if (matrix(row + 1)(col) == '1') {
//
//        } else {
//
//        }
//      }
//    }
//  }
}
