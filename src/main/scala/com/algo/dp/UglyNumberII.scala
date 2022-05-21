package com.algo.dp

object UglyNumberII extends App {
  object Solution {
    def nthUglyNumber(n: Int): Int = {
      val array = new Array[Boolean](2 * n + 1)
      @scala.annotation.tailrec
      def markUglyNums(currN: Int): Unit = {
        if(currN == array.length) {
          ()
        }else if(currN == 1 || currN == 2 || currN == 3 || currN == 5) {
          array(currN) = true
          markUglyNums(currN + 1)
        } else if (currN % 2 == 0 && array(currN / 2)) {
          array(currN) = true
          markUglyNums(currN + 1)
        } else if (currN % 3 == 0 && array(currN / 3)) {
          array(currN) = true
          markUglyNums(currN + 1)
        } else if (currN % 5 == 0 && array(currN / 5)) {
          array(currN) = true
          markUglyNums(currN + 1)
        } else {
          array(currN) = false
          markUglyNums(currN + 1)
        }
      }

      @scala.annotation.tailrec
      def find(index: Int, currCount: Int): Int = {
        if(currCount == n) {
          currCount
        } else if (array(index)) {
          find(index + 1, currCount + 1)
        } else {
          find(index + 1, currCount)
        }
      }

      markUglyNums(1)
      find(0, 0)
    }
  }
}
