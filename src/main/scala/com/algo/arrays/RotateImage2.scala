package com.algo.arrays

object RotateImage2 extends App {

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
      val matLen = matrix.length
      val mid = matLen / 2

      @scala.annotation.tailrec
      def loopCol(row: Int, col: Int, maxColIndex: Int, len2: Int): Unit = {
        if(col == maxColIndex) {
          ()
        } else {
          val temp = matrix(row)(col)
          matrix(row)(col) = matrix(len2)(row)
          matrix(len2)(row) = matrix(maxColIndex)(len2)
          matrix(maxColIndex)(len2) = matrix(col)(maxColIndex)
          matrix(col)(maxColIndex) = temp
          loopCol(row, col + 1, maxColIndex, len2 - 1)


        }
      }

      @scala.annotation.tailrec
      def loopRow(row: Int, maxColIndex: Int): Unit = {
        if(row == mid) {
          ()
        } else {
          loopCol(row, row, maxColIndex, maxColIndex)
          loopRow(row + 1, maxColIndex - 1)
        }
      }

      loopRow(0, matLen - 1)
    }
  }

}
