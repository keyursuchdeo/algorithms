package com.algo.backtracking

import scala.collection.mutable

object CoinChange extends App {

  val c = Array(1, 2, 5)
  val a = 11
//  val c = Array(27,40,244,168,383)
//  val a = 6989
  //  val c = Array(1, 2)
  //  val a = 3
  val res = Solution.coinChange(c, a)
  println(res)

  object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      var map: mutable.Map[(Int, Int, Int), Int] = mutable.Map[(Int, Int, Int), Int]()
      def count(coinCount: Int, remainingAmount: Int, index: Int): Int = {
        if (remainingAmount == 0) {
          coinCount
        } else if (remainingAmount < 0 || index == coins.length) {
          Int.MaxValue
        } else {
          map.get((coinCount, index, remainingAmount)) match {
            case Some(value) => value
            case _ =>
              val min = Math.min(Math.min(
                count(coinCount + 1, remainingAmount - coins(index), index),
                count(coinCount + 1, remainingAmount - coins(index), index + 1)),
                count(coinCount, remainingAmount, index + 1)
              )
              println(s"coinCount -> $coinCount index -> $index remainingAmount -> $remainingAmount min -> $min")
              map = map + ((coinCount, index, remainingAmount) -> min)
              min
          }
        }
      }

      val a = count(0, amount, 0)
      if (a == Int.MaxValue) -1 else a
    }
  }

}
