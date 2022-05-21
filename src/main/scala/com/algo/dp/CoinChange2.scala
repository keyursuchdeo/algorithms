package com.algo.dp

object CoinChange2 extends App {

  object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      val coinsByAmount = Array.fill[Int](amount + 1)(-1)
      lazy val coinSet = coins.toSet

      @scala.annotation.tailrec
      def fillCoinsByAmount(currAmount: Int): Unit = {
        if (currAmount > amount) {
          ()
        } else {
          if (coinSet.contains(currAmount)) {
            coinsByAmount(currAmount) = 1
            fillCoinsByAmount(currAmount + 1)
          } else {
            val min = findMinCoinCount(currAmount)
            coinsByAmount(currAmount) = if (min == Int.MaxValue) -1 else min
            fillCoinsByAmount(currAmount + 1)
          }
        }
      }

      def findMinCoinCount(amount: Int) = {
        val limit = amount / 2

        @scala.annotation.tailrec
        def find(currAmount: Int, min: Int): Int = {
          if (currAmount > limit) {
            min
          } else {
            if (coinsByAmount(currAmount) == -1 || coinsByAmount(amount - currAmount) == -1) {
              find(currAmount + 1, min)
            } else {
              find(currAmount + 1, Math.min(min, (coinsByAmount(currAmount) + coinsByAmount(amount - currAmount))))
            }
          }
        }
        find(1, Int.MaxValue)
      }

      if (amount == 0) {
        0
      } else {
        fillCoinsByAmount(1)
        coinsByAmount(amount)
      }
    }
  }

}
