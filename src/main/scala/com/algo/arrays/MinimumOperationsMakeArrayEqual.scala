package com.algo.arrays

object MinimumOperationsMakeArrayEqual extends App {
  object Solution {
    def minOperations(n: Int): Int = {
      @scala.annotation.tailrec
      def calculate(index: Int, count: Int): Int = {
        val num = (2 * index) + 1
        if(num >= n) {
          count
        } else {
          calculate(index + 1, count + (n - num))
        }
      }

      calculate(0, 0)

    }
  }
}
