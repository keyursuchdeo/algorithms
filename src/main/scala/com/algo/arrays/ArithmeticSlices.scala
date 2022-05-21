package com.algo.arrays

object ArithmeticSlices extends App {

  object Solution {
    def numberOfArithmeticSlices(A: Array[Int]): Int = {

      val sliceCount = new Array[Int](A.length)

      @scala.annotation.tailrec
      def countSlices(index: Int = 2, prevDiff: Int): Unit = {
        if (index >= A.length) {
          ()
        } else {
          val diff2 = A(index) - A(index - 1)
          if (prevDiff == diff2) {
            sliceCount(index) = sliceCount(index - 1) + 1
            countSlices(index + 1, diff2)
          } else {
            countSlices(index + 1, diff2)
          }
        }
      }

      if(A.length < 3) {
        0
      } else {
        countSlices(prevDiff = A(1) - A(0))
        sliceCount.sum
      }
    }
  }

}
