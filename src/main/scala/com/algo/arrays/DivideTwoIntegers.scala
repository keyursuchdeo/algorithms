package com.algo.arrays

object DivideTwoIntegers extends App {
  object Solution {
    def divide(dividend: Int, divisor: Int): Int = {

      val absDividend: Long = Math.abs(1L * dividend)
      val absDivisor: Long = Math.abs(1L * divisor)

      @scala.annotation.tailrec
      def performOperation(currValue: Long, count: Int): Int = {

        if(currValue == absDividend) {
          count
        } else if(currValue > absDividend) {
          count - 1
        } else {
          performOperation(currValue + absDivisor, count + 1)
        }
      }

      if (dividend == 0) {
        0
      } else if (divisor == 1) {
        dividend
      } else if (divisor == -1) {
        if (absDividend > Int.MaxValue) Int.MaxValue else -dividend
      } else if(absDividend == absDivisor) {
        if((dividend < 0 && divisor < 0) || (dividend > 0 && divisor > 0)) 1 else -1
      } else {
        val result = performOperation(absDivisor, 1)
        if((dividend < 0 && divisor < 0) || (dividend > 0 && divisor > 0)) result else -result
      }
    }
  }
}
