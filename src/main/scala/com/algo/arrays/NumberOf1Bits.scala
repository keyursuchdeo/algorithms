package com.algo.arrays

object NumberOf1Bits extends App {
  object Solution {
    // you need treat n as an unsigned value
    def hammingWeight(n: Int): Int = {
      n.toBinaryString.count(_ == '1')
    }
  }

}
