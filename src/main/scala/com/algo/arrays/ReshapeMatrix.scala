package com.algo.arrays

object ReshapeMatrix extends App {
  object Solution {
    def matrixReshape(mat: Array[Array[Int]], r: Int, c: Int): Array[Array[Int]] = {
      val rows = mat.length
      val cols = mat.headOption.map(_.length).getOrElse(0)

      if (rows * cols == r * c) {
        val reshapedMat = Array.ofDim[Int](r, c)
        @scala.annotation.tailrec
        def reshape(flattenedMat: Array[Int], index: Int, row: Int, col: Int): Unit = {
          if(row == r) {
            ()
          } else if (col == c) {
            reshape(flattenedMat, index, row + 1, 0)
          } else {
            reshapedMat(row)(col) = flattenedMat(index)
            reshape(flattenedMat, index + 1, row, col + 1)
          }
        }

        reshape(mat.flatten, 0, 0, 0)
        reshapedMat
      } else {
        mat
      }

    }
  }
}
