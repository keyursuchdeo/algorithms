package com.algo.arrays

object PrisonCells extends App {
  object Solution {
    def prisonAfterNDays(cells: Array[Int], N: Int): Array[Int] = {
      @scala.annotation.tailrec
      def calcUniqueCellArrangements(prevCellArrangement: Array[Int], arrangements: Set[Seq[Int]], map: Map[Int, Seq[Int]], count: Int): (Map[Int, Seq[Int]], Int) = {
        val newCellArrangement: Seq[Int] = calculateNextCellArrangement(prevCellArrangement)
        if(arrangements.contains(newCellArrangement)) {
          (map, count)
        } else {
          calcUniqueCellArrangements(
            newCellArrangement.toArray,
            arrangements + newCellArrangement,
            map + (count -> newCellArrangement),
            count + 1)
        }
      }

      def calculateNextCellArrangement(currCellArrangement: Array[Int]): Seq[Int] = {
        @scala.annotation.tailrec
        def calculate(index: Int, arrangement: Array[Int]): Seq[Int] = {
          if(index == currCellArrangement.length - 1) {
            arrangement.toSeq
          } else {
            if(currCellArrangement(index - 1) == currCellArrangement(index + 1)) {
              arrangement(index) = 1
            } else {
              arrangement(index) = 0
            }
            calculate(index + 1, arrangement)
          }
        }
        calculate(1, Array.fill[Int](currCellArrangement.length)(0))
      }

      def findNthArrangement(map: Map[Int, Seq[Int]], count: Int): Array[Int] = {
        map((N % count) + 1).toArray
      }

      val (arrangementsByNs, count) = calcUniqueCellArrangements(cells, Set(), Map(), 0)
      println(arrangementsByNs)
      println(count)
      findNthArrangement(arrangementsByNs, count)
    }
  }
}
