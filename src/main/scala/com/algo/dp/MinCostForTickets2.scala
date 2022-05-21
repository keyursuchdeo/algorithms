package com.algo.dp

object MinCostForTickets2 extends App {

  object Solution {
    def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {
      val daysSet = days.toSet
      val minCosts = new Array[Int](395)

      @scala.annotation.tailrec
      def calculate(index: Int): Unit = {
        if (index < 0) {
          ()
        } else {
          if (daysSet.contains(index)) {
            minCosts(index) =
              Math.min(
                Math.min(
                  minCosts(index + 1) + costs(0),
                  minCosts(index + 7) + costs(1)
                ),
                minCosts(index + 30) + costs(2)
              )
          } else {
            minCosts(index) = minCosts(index + 1)
          }
          calculate(index - 1)
        }
      }
      calculate(365)
      minCosts.head
    }
  }

}
