package com.algo.dp

object BestTimeToBuySellStockIV2 extends App {

  //  val p = Array(3,2,6,5,0,3)
  val p = Array(2, 4, 1)
  val res = Solution.maxProfit(2, p)
  println(res)

  object Solution {
    def maxProfit(k: Int, prices: Array[Int]): Int = {

      val dp = Array.fill[Option[Int]](prices.length, k + 1, 2)(None)

      def calculate(index: Int, buy: Int, currK: Int): Int = {
        if (index == prices.length || currK > k) {
          0
        } else if (dp(index)(currK)(buy).isDefined) {
          dp(index)(currK)(buy).get
        } else {
          val value =
            if (buy == 1) {
              Math.max(
                -prices(index) + calculate(index + 1, 0, currK),
                calculate(index + 1, buy, currK)
              )
            } else {
              Math.max(
                prices(index) + calculate(index + 1, 1, currK + 1),
                calculate(index + 1, 0, currK)
              )
            }
          dp(index)(currK)(buy) = Option(value)
          value

        }
      }

      @scala.annotation.tailrec
      def calculateProfit(index: Int, profit: Int): Int = {
        if(index == prices.length) {
          profit
        } else {
          if(prices(index) > prices(index - 1)) {
            calculateProfit(index + 1, profit + (prices(index) - prices(index - 1)))
          } else {
            calculateProfit(index + 1, profit)
          }
        }
      }

      if(prices.length < 2 || k < 1) {
        0
      } else if (k > prices.length / 2) {
        calculateProfit(1, 0)
      } else {
        calculate(0, 1, 1)
      }
    }
  }

}
