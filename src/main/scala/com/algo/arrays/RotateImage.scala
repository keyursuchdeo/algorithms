package com.algo.arrays

object RotateImage extends App {

  val m = Array.ofDim[Int](4, 4)
  m(0) = Array(5, 1, 9, 11)
  m(1) = Array(2, 4, 8, 10)
  m(2) = Array(13, 3, 6, 7)
  m(3) = Array(15, 14, 12, 16)

//  val m = Array.ofDim[Int](3, 3)
//  m(0) = Array(1, 2, 3)
//  m(1) = Array(4, 5, 6)
//  m(2) = Array(7, 8, 9)

//  val m = Array.ofDim[Int](2, 2)
//  m(0) = Array(1, 2)
//  m(1) = Array(3, 4)

  Solution.rotate(m)
  m.foreach(r => {
    println(r.mkString(","))
  })

  object Solution {
    def rotate(matrix: Array[Array[Int]]): Unit = {
      @scala.annotation.tailrec
      def rotateRow(row: Int, col: Int, maxColIndex: Int, minRowIndex: Int): Unit = {
        if(maxColIndex <= row) {
          ()
        } else {
          if(col <= maxColIndex - 1) {
            val temp = matrix(row)(col)
            val (dRow1, dCol1) = calculateDestination(row, col, maxColIndex, minRowIndex)
            val (dRow2, dCol2) = calculateDestination(dRow1, dCol1, maxColIndex, minRowIndex)
            val (dRow3, dCol3) = calculateDestination(dRow2, dCol2, maxColIndex, minRowIndex)
            matrix(row)(col) = matrix(dRow3)(dCol3)
            matrix(dRow3)(dCol3) = matrix(dRow2)(dCol2)
            matrix(dRow2)(dCol2) = matrix(dRow1)(dCol1)
            matrix(dRow1)(dCol1) = temp
            rotateRow(row, col + 1, maxColIndex, minRowIndex)
          } else {
            matrix.foreach(r => {
              println(r.mkString(","))
            })
            println("-----------")
            rotateRow(row + 1, row + 1, maxColIndex - 1, row + 1)
          }
        }
      }

      def calculateDestination(row: Int, col: Int, maxColIndex: Int, minRowIndex: Int): (Int, Int) = {
        (col, calculateCol(row, col, maxColIndex, minRowIndex))
      }

      def calculateCol(row: Int, col: Int, maxColIndex: Int, minRowIndex: Int): Int = {
        val tempRow =
          if(row == col && row < maxColIndex) {
            maxColIndex
          } else if (row == col && row == maxColIndex) {
            minRowIndex
          } else if (row < col) {
            row + maxColIndex
          } else {
            row - maxColIndex
          }
        if(tempRow < 0) {
          Math.abs(tempRow)
        } else if (tempRow > maxColIndex) {
          if(tempRow % maxColIndex == 0) {
            0
          } else {
            maxColIndex - (tempRow % maxColIndex)
          }
        } else {
          tempRow
        }
      }

      rotateRow(0, 0, matrix.length - 1, 0)
    }
  }

}
