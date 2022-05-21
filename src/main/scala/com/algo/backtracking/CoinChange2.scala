package com.algo.backtracking

import scala.collection.mutable

object CoinChange2 extends App {

//  val c = Array(1, 2, 5)
//  val a = 11
  val c = Array(27,40,244,168,383)
  val a = 6989
  //  val c = Array(1, 2)
  //  val a = 3
  val res = Solution.coinChange(c, a)
  println(res)

  object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      var map: mutable.Map[Int, Int] = mutable.Map.empty
      def change(remainingAmount: Int): Int = {
        if(remainingAmount == 0) {
          0
        } else if (remainingAmount < 0) {
          -1
        } else {
          map.get(remainingAmount) match {
            case Some(c) => c
            case _ =>
              var min = Int.MaxValue
              for(coin <- coins) {
                val count = change(remainingAmount - coin)
                if(count >= 0 && count < min) {
                  min = 1 + count
                }
              }
              val finalCount = if (min == Int.MaxValue) -1 else min
              map = map + (remainingAmount -> finalCount)
              map.getOrElse(remainingAmount, -1)
          }
        }
      }
      val a = change(amount)
      println(map.get(amount))
      a
    }
  }

}
