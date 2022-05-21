package com.algo.arrays

object StraightLine extends App {
  object Solution {
    def checkStraightLine(coordinates: Array[Array[Int]]): Boolean = {
      val Array(x1, y1) = coordinates(0)
      val Array(x2, y2) = coordinates(1)
      val m: Double = (y2 - y1).toDouble / (x2 - x1)
      val c: Double = y1 - m*x1

      coordinates.tail.tail.find(coordinate =>
        coordinate(1) - m*coordinate(0) != c
      ) match {
        case Some(_) => false
        case _ => true
      }
    }
  }
}
