package com.algo.arrays

object BrokenCalculator extends App {
  object Solution {
    def brokenCalc(X: Int, Y: Int): Int = {
      @scala.annotation.tailrec
      def find(currY: Int, ans: Int): (Int, Int) = {
        if(currY <= X) {
          (ans, currY)
        } else {
          if (currY % 2 == 1) {
            find(currY + 1, ans + 1)
          } else {
            find(currY / 2, ans + 1)
          }
        }
      }

      val (output, updatedY) = find(Y, 0)
      output + X - updatedY
    }
  }
}
