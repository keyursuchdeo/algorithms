package com.algo.heap

import com.algo.arrays.MinimumNumberOfRefuelingStops.Solution

object MinimumNumberOfRefuelingStops extends App {
  val t = 1000
  val s = 299
  val st = Array[Array[Int]](Array(13,21),Array(26,115),Array(100,47),Array(225,99),Array(299,141),Array(444,198),Array(608,190),Array(636,157),Array(647,255),Array(841,123))
  val res = Solution.minRefuelStops(t, s, st)
  println(res)

  object Solution {
    def minRefuelStops(target: Int, startFuel: Int, stations: Array[Array[Int]]): Int = {
      object MaxOrder extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(xD, xF) = x
          val Array(yD, yF) = y
          xF compare yF
        }
      }
      val maxHeap = scala.collection.mutable.PriorityQueue.empty(MaxOrder)
      val updatedStations = (Array(0, startFuel) +: stations) :+ Array(target, 0)

      @scala.annotation.tailrec
      def calculate(index: Int, remainingFuel: Int, stopCount: Int): Int = {
        if(index == updatedStations.length) {
          stopCount
        } else {
          val Array(distance, fuelAvailable) = updatedStations(index)
          val Array(prevDistance, _) = updatedStations(index - 1)
          val fuelConsumed = distance - prevDistance
          if(fuelConsumed > remainingFuel) {
            if(maxHeap.isEmpty) {
              -1
            } else {
              val Array(_, refuelAvailable) = maxHeap.dequeue()
              calculate(index, remainingFuel + refuelAvailable, stopCount + 1)
            }
          } else {
            maxHeap.enqueue(Array(distance, fuelAvailable))
            calculate(index + 1, remainingFuel - fuelConsumed, stopCount)

          }
        }
      }

      calculate(1, startFuel, 0)
    }
  }
}
