package com.algo.dp

object MinDaysToEatOranges extends App {

  object Solution {
    def minDays(n: Int): Int = {

      var map: Map[Int, Int] = Map[Int, Int]()

      def calculateMinDays(currN: Int): Int = {
        if(currN <= 1) {
          currN
        } else {
          map.get(currN) match {
            case Some(value) => value
            case None =>
              val value =
                1 + 
                Math.min(
                  currN % 2 + calculateMinDays(currN / 2),
                  currN % 3 + calculateMinDays(currN / 3),
                )
              map = map + (currN -> value)
              value
          }
        }
      }


      calculateMinDays(n)
      map(n)
    }
  }

}
