package com.algo.dp

import scala.collection.mutable

object StockSellCoolDown2 extends App {

  object Solution {
    def maxProfit(prices: Array[Int]): Int = {

      //One of the dims is 2 since there are 2 possibilities - buy or sell for any given day
      val dp = Array.ofDim[Int](prices.length, 2)

      @scala.annotation.tailrec
      def calculate(index: Int): Unit = {
        if(index == prices.length) {
          ()
        } else {
          if(index == 0) {
            dp(index)(0) = 0;
            dp(index)(1) = - prices(index)
            calculate(index + 1)
          } else if (index == 1) {
            dp(index)(0) = Math.max(dp(index - 1)(0), dp(index - 1)(1) + prices(index))
            dp(index)(1) = Math.max(dp(index - 1)(1), dp(0)(0) - prices(index))
            calculate(index + 1)
          } else {
            dp(index)(0) = Math.max(dp(index - 1)(0), dp(index - 1)(1) + prices(index))
            dp(index)(1) = Math.max(dp(index - 1)(1), dp(index - 2)(0) - prices(index))
            calculate(index + 1)
          }
        }
      }

      if(prices.length <= 1) {
        0
      } else if (prices.length == 2 && prices(1) > prices(0)) {
        prices(1) - prices(0)
      } else if (prices.length == 2 && prices(1) <= prices(0)) {
        0
      } else {
        calculate(0)
        dp(prices.length - 1)(0)
      }
    }
  }

}
