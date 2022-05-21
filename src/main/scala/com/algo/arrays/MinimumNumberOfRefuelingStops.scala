package com.algo.arrays

object MinimumNumberOfRefuelingStops extends App {

  val t = 100
  val s = 50
  val st = Array[Array[Int]](Array(25,50),Array(50,25))
  val res = Solution.minRefuelStops(t, s, st)
  println(res)

  object Solution {
    def minRefuelStops(target: Int, startFuel: Int, stations: Array[Array[Int]]): Int = {
      val updatedStations = (Array(0, startFuel) +: stations) :+ Array(target, 0)
      var map: Map[(Int, Int), Long] = Map[(Int, Int), Long]()
      def calculate(index: Int = 1, remainingFuel: Int): Long = {
        if(index == updatedStations.length) {
          0
        } else {
          val Array(distance, fuelAvailable) = updatedStations(index)
          val Array(prevDistance, _) = updatedStations(index - 1)
          val fuelConsumed = distance - prevDistance
          if(fuelConsumed > remainingFuel) {
            Int.MaxValue
          } else {
            if (distance == target) {
              0
            } else {
              map.get((index, remainingFuel)) match {
                case Some(value) =>
                  value
                case _ =>
                  val value =
                    if (fuelConsumed < remainingFuel) {
                      Math.min(
                        calculate(index + 1, remainingFuel - fuelConsumed),
                        1 + calculate(index + 1, remainingFuel - fuelConsumed + fuelAvailable)
                      )
                    } else {
                      1 + calculate(index + 1, fuelAvailable)
                    }
                  map = map + ((index, remainingFuel) -> value)
                  value
              }
            }
          }
        }
      }

      val stops = calculate(remainingFuel = startFuel)
      if(stops == Int.MaxValue) {
        -1
      } else {
        stops.toInt
      }
    }
  }
}
