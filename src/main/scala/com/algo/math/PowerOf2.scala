package com.algo.math

object PowerOf2 extends App {
  object Solution {
    def isPowerOfTwo(n: Int): Boolean = {
      val pow: Double = Math.log10(n) / Math.log10(2)
      pow == pow.toInt
    }
  }
}
