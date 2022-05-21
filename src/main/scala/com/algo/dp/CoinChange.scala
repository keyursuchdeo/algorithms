package com.algo.dp

import scala.util.{Failure, Success, Try}

object CoinChange extends App {

  object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      val change = new Array[Int](amount + 1)
      val coinsSet = coins.toSet

      var changeMap: Map[Int, Int] = Map[Int, Int]()

      def dp(currAmount: Int): Int = {
        if (currAmount == 0) {
          0
        } else if (currAmount < 0) {
          -1
        } else {
          changeMap.get(currAmount) match {
            case Some(value) => value
            case None =>
              val value =
                coins.map(coin => {
                  val currOutput = dp(currAmount - coin)
                  if(currOutput == -1) Int.MaxValue else 1 + currOutput
                }).min
              changeMap = changeMap + (currAmount -> value)
              value
          }
        }
      }

      val output = dp(amount)
      if (output == Int.MaxValue) -1 else output

      @scala.annotation.tailrec
      def fillChange(index: Int): Unit = {
        if (index == change.length) {
          ()
        } else {
          if (coinsSet.contains(index)) {
            change(index) = 1
          } else {
            change(index) =
              Try((1 until index).collect {
                case i if change(i) != -1 && change(index - i) != -1 =>
                  change(i) + change(index - i)
              }.min) match {
                case Success(min) => min
                case Failure(_) => -1
              }
          }
          fillChange(index + 1)
        }
      }

      fillChange(1)
      change(amount)
    }
  }

}
