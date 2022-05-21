package com.algo.dp

import scala.collection.mutable

object StockSellCoolDown extends App {

  object Solution {
    def maxProfit(prices: Array[Int]): Int = {

      object StockAction extends Enumeration {
        type StockAction = Value
        val BUY, SELL, COOLDOWN = Value
      }

      import StockAction._

      var map: mutable.Map[(Int, StockAction.StockAction, Int), Int] = mutable.Map[(Int, StockAction, Int), Int]()

      def calculate(index: Int, prevAction: StockAction, profitTillNow: Int): Int = {
        if (index == prices.length) {
          profitTillNow
        } else {
          map.get((index, prevAction, profitTillNow)) match {
            case Some(value) => value
            case None =>
              val value =
                if (prevAction == BUY) {
                  Math.max(
                    calculate(index + 1, SELL, profitTillNow + prices(index)),
                    calculate(index + 1, prevAction, profitTillNow)
                  )
                } else if (prevAction == SELL) {
                  calculate(index + 1, COOLDOWN, profitTillNow)
                } else {
                  Math.max(
                    calculate(index + 1, BUY, profitTillNow - prices(index)),
                    calculate(index + 1, prevAction, profitTillNow)
                  )
                }
              map = map + ((index, prevAction, profitTillNow) -> value)
              value
          }
        }
      }

      calculate(0, COOLDOWN, 0)
    }
  }

}
