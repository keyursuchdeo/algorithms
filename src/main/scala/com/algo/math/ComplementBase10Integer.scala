package com.algo.math

object ComplementBase10Integer extends App {
  object Solution {
    def bitwiseComplement(N: Int): Int = {
      if(N == 0) 1 else {
        val numOfBits: Int = Math.floor(Math.log10(N) / Math.log10(2)).toInt + 1
        val num = Math.pow(2, numOfBits) - 1
        (num - N).toInt
      }
    }
  }
}
