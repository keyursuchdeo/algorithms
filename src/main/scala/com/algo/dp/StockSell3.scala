package com.algo.dp

object StockSell3 extends App {

//  val a = Array(3, 3, 5, 0, 0, 3, 1, 4)
//  val a = Array(5, 11, 3, 50, 60, 90)
    val a = Array(1,2,3,4,5)
  val res = Solution.maxProfit(2, a)
  println(res)

  object Solution {
    def maxProfit(k: Int, prices: Array[Int]): Int = {
      val numOfTx = k
      val profitsByTx: Array[Array[Int]] = Array.ofDim[Int](2, prices.length)

      @scala.annotation.tailrec
      def max(priceIndex: Int, txIndex: Int, addlProfit: Int): Array[Array[Int]] = {
        if (txIndex == numOfTx + 1) {
          profitsByTx
        } else if (priceIndex == prices.length) {
          profitsByTx(0) = profitsByTx(1)
          max(1, txIndex + 1, -prices(0))
        } else {
          val maxAddlProfit = Math.max(addlProfit, -prices(priceIndex - 1) + profitsByTx(0)(priceIndex - 1))
          profitsByTx(1)(priceIndex) = Math.max(
            profitsByTx(1)(priceIndex - 1),
            prices(priceIndex) + maxAddlProfit
          )
          max(priceIndex + 1, txIndex, maxAddlProfit)
        }
      }

      if(prices.length == 0) 0 else {
        val updatedProfitsByTx = max(1, 1, -prices(0))
        val a: Array[String] = updatedProfitsByTx.map(_.mkString(","))
        println(a.mkString("|"))
        updatedProfitsByTx(0)(prices.length - 1)
      }
    }
  }
}
