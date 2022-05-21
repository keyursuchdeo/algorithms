package com.algo.dp

object MinCostForTickets extends App {

  object Solution {
    def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {
      val costsWithDays = costs.zip(Array(1, 7, 30))
      val minCosts = new Array[Int](days.length)
      val remainingTktDays = new Array[Int](days.length)


      def find(index: Int, remainingDays: Int): Int = {
        if (index == days.length) {
          0
        } else {
          if (remainingDays == 0) {
            Math.min(
              costs(0) + find(index + 1, 0),
              Math.min(
                costs(1) + find(index + 1, 6),
                costs(2) + find(index + 1, 29)
              )
            )
          } else {
            if (days(index) - days(index - 1) < remainingDays - days(index)) {
              find(index + 1, remainingDays - days(index))
            } else {
              Math.min(
                costs(0) + find(index + 1, 0),
                Math.min(
                  costs(1) + find(index + 1, 6),
                  costs(2) + find(index + 1, 29)
                )
              )
            }
          }
        }
      }

      find(0, 0)
    }
  }

}
