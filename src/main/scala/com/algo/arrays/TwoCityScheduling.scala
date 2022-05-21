package com.algo.arrays

object TwoCityScheduling extends App {
  object Solution {
    def twoCitySchedCost(costs: Array[Array[Int]]): Int = {
      val refund = new Array[Int](costs.length)

      @scala.annotation.tailrec
      def fill(index: Int, minCost: Int): Int = {
        if(index == costs.length) {
          minCost
        } else {
          refund(index) = costs(index)(1) - costs(index)(0)
          fill(index + 1, minCost + costs(index)(0))
        }
      }

      val currMinCost = fill(0, 0)
      currMinCost + refund.sorted.take(costs.length / 2).sum
    }
  }
}
