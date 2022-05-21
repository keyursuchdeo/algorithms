package com.algo.arrays

object StockSpanner extends App {
//  val a = Array(100, 80, 60, 70, 60, 75, 85)
  val a = Array(6, 5, 4, 3, 2, 1)
  val stockSpanner = new StockSpanner()
  println(a.map(stockSpanner.next).mkString(","))
}

class StockSpanner {

  private var prices: Seq[Int] = Seq[Int]()
  private var spans: Seq[Int] = Seq[Int]()
  private var numOfPrices = 0

  def next(price: Int): Int = {
    prices.headOption match {
      case Some(value) if value <= price =>
        val span = calculateSpan(price)
        spans = span +: spans
        prices = price +: prices
        numOfPrices = numOfPrices + 1
        span
      case _ =>
        prices = price +: prices
        spans = 1 +: spans
        numOfPrices = numOfPrices + 1
        1

    }
  }

  private def calculateSpan(price: Int): Int = {
    @scala.annotation.tailrec
    def calculate(index: Int, span: Int): Int = {
      if (index == numOfPrices || prices(index) > price) {
        span
      } else {
        calculate(index + spans(index), span + spans(index))
      }
    }
    calculate(0, 1)
  }
}
