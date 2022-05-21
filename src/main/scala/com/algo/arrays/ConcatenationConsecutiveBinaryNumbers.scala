package com.algo.arrays

object ConcatenationConsecutiveBinaryNumbers extends App {
  object Solution {
    def concatenatedBinary(n: Int): Int = {

      val Mod: Int = Math.pow(10, 9).toInt + 7

      @scala.annotation.tailrec
      def calculate(currN: Int, accBinLen: Int, currSum: Int): Int = {
        if(currN == 1) {
          val num = currN * Math.pow(2, 1 + accBinLen)
          ((currSum + num) % Mod).toInt
        } else {
          if(currN == n) {
            calculate(currN - 1, 0, currN)
          } else {
            val binLen = (currN + 1).toBinaryString.length
            val num = currN * Math.pow(2, binLen + accBinLen)
            calculate(currN - 1, binLen + accBinLen, ((currSum + num) % Mod).toInt)
          }
        }
      }

      calculate(n, 0, 0)
    }
  }
}
