package com.algo.arrays

object MinCostMoveChipsSamePosition2 extends App {

  object Solution {
    def minCostToMoveChips(position: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(index: Int, oddCount: Int, evenCount: Int): Int = {
        if (index == position.length) {
          if(oddCount == 0 || evenCount == 0) {
            0
          } else {
            Math.min(oddCount, evenCount)
          }
        } else {
          if (position(index) % 2 == 0) {
            find(index + 1, oddCount, evenCount + 1)
          } else {
            find(index + 1, oddCount + 1, evenCount)
          }
        }
      }

      find(0, 0, 0)
    }
  }

}
