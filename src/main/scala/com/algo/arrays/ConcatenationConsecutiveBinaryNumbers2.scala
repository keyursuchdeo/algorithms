package com.algo.arrays

object ConcatenationConsecutiveBinaryNumbers2 extends App {
  object Solution {
    def concatenatedBinary(n: Int): Int = {

      val Mod: Int = Math.pow(10, 9).toInt + 7

      @scala.annotation.tailrec
      def calculate(currN: Int, currSum: Int): Int = {
        if(currN > n) {
          currSum
        } else {
          val binLen = currN.toBinaryString.length
          val updatedSum =
            ((currSum.toLong << binLen) + currN) % Mod
          calculate(currN + 1, updatedSum.toInt)
        }
      }

      calculate(1, 0)
    }
  }
}
