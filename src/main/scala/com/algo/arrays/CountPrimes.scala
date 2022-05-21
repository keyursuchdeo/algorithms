package com.algo.arrays

object CountPrimes extends App {
  object Solution {
    def countPrimes(n: Int): Int = {
      def isComposite(num: Int) = {
        val upperLimit = Math.sqrt(num).toInt
        if(upperLimit <= 1) false else (2 to upperLimit).exists(f => num % f == 0)
      }

      @scala.annotation.tailrec
      def check(currN: Int, count: Int): Int = {
        if(currN == n) {
          count
        } else {
          if(isComposite(currN)) {
            check(currN + 1, count)
          } else {
            check(currN + 1, count + 1)
          }
        }
      }

      check(2, 0)
    }
  }
}
