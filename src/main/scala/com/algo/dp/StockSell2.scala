package com.algo.dp

object StockSell2 extends App {

//  val a = Array(7,2,5,3,6,4,1,8)
//  val a = Array(1, 2, 3, 4, 5)
//  val a = Array(2, 1, 2, 1, 0, 1, 2)
  val a = Array(3,3,5,0,0,3,1,4)
//  val a = Array(7,1,5,3,6,4)
//  val a = Array(1,2,3,4,5)
//  val a = Array(7,6,4,3,1)
  val res = Solution.maxProfit(a)
  println(res)

  object Solution {
    def maxProfit(prices: Array[Int]): Int = {
      @scala.annotation.tailrec
      def max1(index: Int, minBuyingPrice: Int, maxSellingPrice: Option[Int], totalProfit: Int): Int = {
        if(index == prices.length) {
          maxSellingPrice match {
            case Some(p) => totalProfit + (p - minBuyingPrice)
            case _ => totalProfit
          }
        } else {
          maxSellingPrice match {
            case Some(sp) =>
              if(prices(index) >= sp) {
                max1(index + 1, minBuyingPrice, Some(prices(index)), totalProfit)
              } else {
                max1(index + 1, prices(index), None, totalProfit + (sp - minBuyingPrice))
              }
            case None =>
              if(prices(index) <= minBuyingPrice) {
                max1(index + 1, prices(index), maxSellingPrice, totalProfit)
              } else {
                max1(index + 1, minBuyingPrice, Some(prices(index)), totalProfit)
              }
          }
        }
      }

      if(prices.isEmpty) 0 else max1(1, prices(0), None, 0)
    }
  }
}
