package com.algo.arrays

object KthFactorOfN extends App {
  object Solution {
    def kthFactor(n: Int, k: Int): Int = {
      @scala.annotation.tailrec
      def findFactors(f: Int, count: Int): Int = {
        if (f == n) {
          if(count + 1 == k) {
            f
          } else {
            -1
          }
        } else {
          if (n % f == 0) {
            if(count + 1 == k) {
              f
            } else {
              findFactors(f + 1, count + 1)
            }
          } else {
            findFactors(f + 1, count)
          }
        }
      }

      findFactors(1, 0)
    }
  }
}
