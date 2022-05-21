package com.algo.dp

object BestTimeToBuySellStockIV extends App {

//  val p = Array(3,2,6,5,0,3)
  val p = Array(2, 4, 1)
  val res = Solution.maxProfit(2, p)
  println(res)

  object Solution {
    def maxProfit(k: Int, prices: Array[Int]): Int = {
      var prevProfit = new Array[Int](prices.length + 1)
      val currProfit = new Array[Int](prices.length + 1)

      @scala.annotation.tailrec
      def fillProfits(priceIndex: Int = 1, currK: Int): Unit = {
        if(priceIndex == prices.length + 1 && currK == k){
          ()
        } else if (priceIndex == prices.length + 1) {
          prevProfit = currProfit
          fillProfits(1, currK + 1)
        } else {
          if (priceIndex < 2 * currK) {
            currProfit(priceIndex) = prevProfit(priceIndex)
            fillProfits(priceIndex + 1, currK)
          } else {
            currProfit(priceIndex) =
              Math.max(
                Math.max(
                  prevProfit(priceIndex),
                  currProfit(priceIndex - 1)
                ),
                (1 until priceIndex).map(pI => {
                  prevProfit(pI - 1) + (prices(priceIndex - 1) - prices(pI - 1))
                }).max
              )
            fillProfits(priceIndex + 1, currK)
          }
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
        fillProfits(1, 1)
//        println(prevProfit.mkString(","))
//        println(currProfit.mkString(","))
        currProfit(prices.length)
      }
    }
  }
}
