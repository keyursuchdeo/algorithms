package com.algo.greedy
import scala.collection.immutable

object GasStation extends App {
//  val g = Array(1, 2, 3, 4, 5)
//  val c = Array(3, 4, 5, 1, 2)

//  val g = Array(2, 3, 4)
//  val c = Array(3, 4, 3)

//  val g = Array(5, 5, 1, 3, 4)
//  val c = Array(8, 1, 7, 1, 1)

//  val g = Array(5, 1, 2, 3, 4)
//  val c = Array(4, 4, 1, 5, 1)

  val g = Array(1, 2, 3, 4, 5, 5, 70)
  val c = Array(2, 3, 4, 3, 9, 6, 2)

  println(Solution.canCompleteCircuit(g, c))

  object Solution {
    def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
      val gasCostDiffs: immutable.IndexedSeq[Int] = gasMinusCost(gas, cost)

      @scala.annotation.tailrec
      def find(currIndex: Int, startIndex: Int = -1, gasInTank: Int = 0, skippedDiffs: Int = 0): Int = {
        if(currIndex == gas.length) {
          println(gasInTank)
          println(skippedDiffs)
          if(gasInTank + skippedDiffs >= 0) startIndex else -1
        } else {
          if(gasInTank + gasCostDiffs(currIndex) < 0) {
            find(currIndex + 1, startIndex = -1, skippedDiffs = skippedDiffs + gasCostDiffs(currIndex) + gasInTank)
          } else if (gasInTank > 0){
            find(currIndex + 1, startIndex, gasInTank + gasCostDiffs(currIndex), skippedDiffs)
          } else {
            find(currIndex + 1, currIndex, gasInTank + gasCostDiffs(currIndex), skippedDiffs)
          }
        }
      }

      find(0)
    }

    private def gasMinusCost(gas: Array[Int], cost: Array[Int]): immutable.IndexedSeq[Int] =
      gas.indices.map(i => gas(i) - cost(i))
  }
}
