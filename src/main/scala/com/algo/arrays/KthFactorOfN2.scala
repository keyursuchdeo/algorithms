package com.algo.arrays

object KthFactorOfN2 extends App {
  object Solution {
    def kthFactor(n: Int, k: Int): Int = {
      val sqRootN: Double = Math.sqrt(n)

      @scala.annotation.tailrec
      def findFactors(possibleFactor: Int = 1, factors1: Seq[Int] = Nil, factors2: Seq[Int] = Nil): Array[Int] = {
        if(possibleFactor > sqRootN) {
          (factors1.reverse ++ factors2).toArray
        } else {
          if(n % possibleFactor == 0) {
            if(possibleFactor == n / possibleFactor) {
              findFactors(possibleFactor + 1, possibleFactor +: factors1, factors2)
            } else {
              findFactors(possibleFactor + 1, possibleFactor +: factors1, n / possibleFactor +: factors2)
            }
          } else {
            findFactors(possibleFactor + 1, factors1, factors2)
          }
        }
      }

      val allFactors = findFactors()
      if(allFactors.length <= k) allFactors(k - 1) else -1
    }
  }
}
