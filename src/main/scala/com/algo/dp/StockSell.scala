package com.algo.dp

object StockSell extends App {

//  val a = Array(7,2,5,3,6,4,1,8)
//  val a = Array(1, 2, 3, 4, 5)
//  val a = Array(2, 1, 2, 1, 0, 1, 2)
  val a = Array(3,3,5,0,0,3,1,4)
  val res = Solution.maxProfit(a)
  println(res)

  object Solution {
    def maxProfit(prices: Array[Int]): Int = {

      @scala.annotation.tailrec
      def max1(index: Int, minBuyingPrice: Int, currBuyingPrice: Int, maxSellingPrice: Option[Int]): Int = {
        if(index == prices.length) {
          maxSellingPrice match {
            case Some(p) => p - minBuyingPrice
            case _ => 0
          }
        } else {
          maxSellingPrice match {
            case Some(sp) =>
              if(prices(index) >= sp || (prices(index) - currBuyingPrice > (sp - minBuyingPrice))) {
                max1(index + 1, currBuyingPrice, currBuyingPrice, Some(prices(index)))
              } else if(prices(index) <= currBuyingPrice) {
                max1(index + 1, minBuyingPrice, prices(index), maxSellingPrice)
              } else {
                max1(index + 1, minBuyingPrice, currBuyingPrice, maxSellingPrice)
              }
            case None =>
              if(prices(index) <= minBuyingPrice) {
                max1(index + 1, prices(index), prices(index), maxSellingPrice)
              } else {
                max1(index + 1, minBuyingPrice, minBuyingPrice, Some(prices(index)))
              }
          }
        }
      }

      if(prices.isEmpty) 0 else max1(1, prices(0), prices(0), None)
    }
  }
}
