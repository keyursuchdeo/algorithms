package com.algo.arrays

object SmallestIntegerDivisibleByK extends App {
  object Solution {
    def smallestRepunitDivByK(K: Int): Int = {
      @scala.annotation.tailrec
      def find(remainder: Int, length: Int, index: Int): Int = {
        if(remainder % K == 0) {
          length
        } else if (index == K) {
          -1
        } else {
          find((remainder * 10 + 1) % K, length + 1, index + 1)
        }
      }

      find(1, 1, 0)
    }
  }
}
