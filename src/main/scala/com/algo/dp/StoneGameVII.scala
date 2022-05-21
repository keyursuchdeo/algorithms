package com.algo.dp

import scala.collection.mutable

object StoneGameVII extends App {
  object Solution {
    def stoneGameVII(stones: Array[Int]): Int = {
      val cumulativeSum = new Array[Int](stones.length + 1)
      var map = mutable.Map[(Int, Int), Int]()
      @scala.annotation.tailrec
      def fillCumulativeSum(index: Int): Unit = {
        if(index == stones.length) {
          ()
        } else {
          cumulativeSum(index + 1) = stones(index) + cumulativeSum(index)
          fillCumulativeSum(index + 1)
        }
      }
      def calculate(low: Int, high: Int): Int = {
        if(low == high) {
          0
        } else {
          map.get((low, high)) match {
            case Some(value) =>
              value
            case None =>
              val sum = cumulativeSum(high + 1) - cumulativeSum(low)
              val value = sum - Math.min(stones(low) + calculate(low + 1, high), stones(high) + calculate(low, high - 1))
              map = map + ((low, high) -> value)
              value
          }
        }
      }

      fillCumulativeSum(0)
      calculate(0, stones.length - 1)
    }
  }
}
