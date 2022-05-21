package com.algo.dp

object MaximumUnitsOnATruck2 extends App {
  object Solution {
    def maximumUnits(boxTypes: Array[Array[Int]], truckSize: Int): Int = {
      object UnitOrder extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(_, xUnitsPerBox) = x
          val Array(_, yUnitsPerBox) = y
          yUnitsPerBox compare xUnitsPerBox
        }
      }

      val sortedBoxTypes: Array[Array[Int]] = boxTypes.sorted(UnitOrder)

      @scala.annotation.tailrec
      def calculate(index: Int, boxesOfType: Int, totalUnits: Int, remainingBoxes: Int): Int = {
        if(index == boxTypes.length || remainingBoxes == 0) {
          totalUnits
        } else {
          val Array(numOfBoxes, numOfUnitsPerBox) = sortedBoxTypes(index)
          if(boxesOfType == numOfBoxes) {
            calculate(index + 1, 0, totalUnits, remainingBoxes)
          } else {
            calculate(index, boxesOfType + 1, totalUnits + numOfUnitsPerBox, remainingBoxes - 1)
          }
        }
      }

      calculate(0, 0, 0, truckSize)
    }
  }
}
