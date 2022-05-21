package com.algo.dp

object MaximumUnitsOnATruck extends App {
  object Solution {
    def maximumUnits(boxTypes: Array[Array[Int]], truckSize: Int): Int = {
      val unitCount = Array.fill[Int](boxTypes.length, truckSize + 1)(-1)
      def calculate(index: Int, remainingTruckSize: Int): Int = {
        if(index == boxTypes.length || remainingTruckSize == 0) {
          0
        } else {
          if(unitCount(index)(remainingTruckSize) != -1) {
            unitCount(index)(remainingTruckSize)
          } else {
            val Array(numOfBoxes, numOfUnitsPerBox) = boxTypes(index)
            val units =
              (0 to numOfBoxes).collect {
                case boxCount if remainingTruckSize - boxCount >= 0 =>
                  val totalUnits = boxCount * numOfUnitsPerBox
                  totalUnits + calculate(index + 1, remainingTruckSize - boxCount)
              }
            unitCount(index)(remainingTruckSize) = if (units.isEmpty) 0 else units.max
            unitCount(index)(remainingTruckSize)
          }
        }
      }

      calculate(0, truckSize)
    }
  }
}
