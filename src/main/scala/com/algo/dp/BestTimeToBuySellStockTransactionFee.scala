package com.algo.dp

object BestTimeToBuySellStockTransactionFee extends App {
  object Solution {
    def maxProfit(prices: Array[Int], fee: Int): Int = {

      @scala.annotation.tailrec
      def calculate(index: Int = 1, cash: Int, hold: Int): Int = {
        if(index == prices.length) {
          cash
        } else {
          calculate(
            index + 1,
            Math.max(cash, hold + prices(index) - fee),
            Math.max(hold, cash - prices(index))
          )
        }
      }

      calculate(1, 0, -prices(0))
    }
  }
}
