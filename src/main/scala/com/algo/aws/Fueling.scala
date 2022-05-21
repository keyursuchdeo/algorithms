package com.algo.aws

object Fueling extends App {

}

class SolutionFuel {
  def solution(n: Array[Int], x: Int, y: Int, z: Int): Int = {

    def waitTime(index: Int = 0, waitTime: Int = 0, remX: Dispenser, remY: Dispenser, remZ: Dispenser): Int = {
      if (index == n.length) {
        waitTime
      } else {
        val dispensers: Option[(Dispenser, Dispenser, Dispenser)] = chooseDispenser(n(index), remX, remY, remZ)
        if(dispensers.isEmpty) {
          -1
        } else {
          //waitTime(index + 1, waitTime + n(index), dispensers.get._1, dispensers.get._2, dispensers.get._3)
          1
        }
      }
    }

    val xD = Dispenser("x", x)
    val yD = Dispenser("y", y)
    val zD = Dispenser("z", z)
    waitTime(remX = xD, remY = yD, remZ = zD)
  }

  private def chooseDispenser(fuelCapacity: Int, x: Dispenser, y: Dispenser, z: Dispenser): Option[(Dispenser, Dispenser, Dispenser)] = {
    if (x.capacity >= fuelCapacity) {
      Option(x.copy(capacity = x.capacity - fuelCapacity), y, z)
    } else if (y.capacity >= fuelCapacity) {
      Option(x, y.copy(capacity = y.capacity - fuelCapacity), z)
    } else if (z.capacity >= fuelCapacity) {
      Option(x, y, z.copy(capacity = z.capacity - fuelCapacity))
    } else {
      None
    }
  }

}

case class Dispenser(dispenser: String, capacity: Int)
