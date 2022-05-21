package com.algo.arrays

object ValidPerfectSquare extends App {
  object Solution {
    def isPerfectSquare(num: Int): Boolean = {
      @scala.annotation.tailrec
      def find(i: Int): Boolean = {
        if (i == 1) {
          false
        } else {
          if (i * i == num) true else find(i - 1)
        }
      }

      if (num == 1) true else {
        val halfNum = num / 2
        find(halfNum)
      }
    }
  }
}
