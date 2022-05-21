package com.algo.arrays

import scala.util.Try

object ImageOverlap extends App {

  val a = Array.ofDim[Int](3, 3)
  a(0) = Array(1, 1, 0)
  a(1) = Array(0, 1, 0)
  a(2) = Array(0, 1, 0)

  val b = Array.ofDim[Int](3, 3)
  b(0) = Array(0, 0, 0)
  b(1) = Array(0, 1, 1)
  b(2) = Array(0, 0, 1)

  val res = Solution.largestOverlap(a, b)
  println(res)

  object Solution {
    def largestOverlap(A: Array[Array[Int]], B: Array[Array[Int]]): Int = {

      def countOverlap(x: Int, y: Int, shift: Array[Array[Int]], other: Array[Array[Int]]): Int = {
        @scala.annotation.tailrec
        def count(row: Int, col: Int, overlap: Int): Int = {
          if(row == A.length) {
            overlap
          } else if (col == A.length) {
            count(row + 1, 0, overlap)
          } else {
            if(Try(shift(row - y)(col - x)) == Try(other(row)(col)) && other(row)(col) == 1) {
              count(row, col + 1, overlap + 1)
            } else {
              count(row, col + 1, overlap)
            }
          }
        }

        count(y, x, 0)
      }

      @scala.annotation.tailrec
      def shift(xShift: Int, yShift: Int, max: Int): Int = {
        if(yShift == A.length) {
          max
        } else if (xShift == A.length) {
          shift(0, yShift + 1, max)
        } else {
          val overlap1 = countOverlap(xShift, yShift, A, B)
          val overlap2 = countOverlap(xShift, yShift, B, A)
          shift(xShift + 1, yShift, Math.max(Math.max(overlap1, overlap2), max))
        }
      }
      shift(0, 0, 0)
    }
  }
}
