package com.algo.arrays

object CapacityToShipPackagesWithinDays extends App {

  val input = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  val res = Solution.shipWithinDays(input, 5)
  println(res)

  object Solution {
    def shipWithinDays(weights: Array[Int], D: Int): Int = {
      def isFeasible(capacity: Int): Boolean = {
        @scala.annotation.tailrec
        def checkFeasibility(days: Int, index: Int, currCapacity: Int): Boolean = {
          if(days > D) {
            false
          } else if (index == weights.length ) {
            true
          } else {
            if(currCapacity + weights(index) > capacity) {
              checkFeasibility(days + 1, index, 0)
            } else {
              checkFeasibility(days, index + 1, currCapacity + weights(index))
            }
          }
        }
        checkFeasibility(1, 0, 0)
      }

      @scala.annotation.tailrec
      def ship(lowestRequiredCapacity: Int, highestPossibleCapacity: Int): Int = {
        if(highestPossibleCapacity < lowestRequiredCapacity) {
          lowestRequiredCapacity
        } else {
          val mid = (highestPossibleCapacity + lowestRequiredCapacity) / 2
          if(isFeasible(mid)) {
            ship(lowestRequiredCapacity, mid - 1)
          } else {
            ship(mid + 1, highestPossibleCapacity)
          }
        }
      }

      ship(weights(weights.length - 1), weights.sum)
    }
  }
}
