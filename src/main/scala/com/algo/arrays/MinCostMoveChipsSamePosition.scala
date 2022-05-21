package com.algo.arrays

object MinCostMoveChipsSamePosition extends App {
  object Solution {
    def minCostToMoveChips(position: Array[Int]): Int = {
        @scala.annotation.tailrec
        def identifyPositionWithMaxChips(index: Int, chipCountByPosition: Map[Int, Int]): Int = {
          if(index == position.length) {
            chipCountByPosition.maxBy(_._2)._1
          } else {
            chipCountByPosition.get(position(index)) match {
              case Some(count) =>
                identifyPositionWithMaxChips(index + 1, chipCountByPosition + (position(index) -> (1 + count)))
              case None =>
                identifyPositionWithMaxChips(index + 1, chipCountByPosition + (position(index) -> 1))
            }
          }
        }

      @scala.annotation.tailrec
      def moveChipsToPosition(index: Int, p: Int, costTillNow: Int): Int = {
        if(index == position.length) {
          costTillNow
        } else {
          if(position(index) == p || Math.abs(p - position(index)) % 2 == 0) {
            moveChipsToPosition(index + 1, p, costTillNow)
          } else {
            moveChipsToPosition(index + 1, p, costTillNow + 1)
          }
        }
      }

      val pos = identifyPositionWithMaxChips(0, Map())
      moveChipsToPosition(0, pos, 0)

    }
  }
}
