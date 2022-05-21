package com.algo.arrays

object FlippingAnImage extends App {
  object Solution {
    def flipAndInvertImage(A: Array[Array[Int]]): Array[Array[Int]] = {
      val rows = A.length
      val cols = A.headOption.map(_.length).getOrElse(0)

      def invert(bin: Int) = bin ^ 1

      def flipAndInvertRow(row: Int): Unit = {
        @scala.annotation.tailrec
        def flip(col: Int): Unit = {
          if (col > cols - col - 1) {
            ()
          } else {
            val temp = A(row)(col)
            A(row)(col) = invert(A(row)(cols - col - 1))
            A(row)(cols - col - 1) = invert(temp)
            flip(col + 1)
          }
        }
        flip(0)
      }

      @scala.annotation.tailrec
      def flipAndInvertAllRows(index: Int): Unit = {
        if(index == rows) {
          ()
        } else {
          flipAndInvertRow(index)
          flipAndInvertAllRows(index + 1)
        }
      }

      flipAndInvertAllRows(0)
      A
    }
  }
}
