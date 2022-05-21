package com.algo.dp

import scala.collection.mutable

object ReachNumber extends App {

  object Solution {
    def reachNumber(target: Int): Int = {
      val updTarget = Math.abs(target)

      @scala.annotation.tailrec
      def reach(currNum: Int, currMoveCount: Int): Int = {
        if(currNum == updTarget) {
          currMoveCount - 1
        } else if (currNum < updTarget) {
          reach(currNum + currMoveCount, currMoveCount + 1)
        } else {
          val delta = currNum - updTarget
          if (delta % 2 == 0) {
            currMoveCount - 1
          } else {
            reach(currNum + currMoveCount, currMoveCount + 1)
          }
        }
      }

      reach(0, 1)
    }
  }

}
