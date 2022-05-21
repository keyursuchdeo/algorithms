package com.algo.arrays

object SuperWashingMachines extends App {
  object Solution {
    def findMinMoves(machines: Array[Int]): Int = {
      val dressCount = machines.sum
      val dressesPerMachine: Int = dressCount / machines.length

      @scala.annotation.tailrec
      def find(index: Int, moveCount: Int): Int = {
        if (index == machines.length) {
          moveCount
        } else {
          if(machines(index) >= dressesPerMachine) {
            find(index + 1, moveCount)
          } else {
            find(index + 1, moveCount + (dressesPerMachine - machines(index)))
          }
        }
      }


      if (dressCount % machines.length != 0) {
        -1
      } else {
        find(0, 0)
      }
    }
  }
}
