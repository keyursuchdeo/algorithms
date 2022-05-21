package com.algo.arrays

object HammingDistance2 extends App {

  val res = Solution.hammingDistance(1, 4)
  println(res)

  object Solution {
    def hammingDistance(x: Int, y: Int): Int = {
      Integer.bitCount(x ^ y)
    }
  }

}
