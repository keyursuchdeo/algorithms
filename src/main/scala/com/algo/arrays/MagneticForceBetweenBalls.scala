package com.algo.arrays

import scala.collection.mutable

object MagneticForceBetweenBalls extends App {
  object Solution {
    def maxDistance(position: Array[Int], m: Int): Int = {
      val sortedPositions = position.sorted
      val queue: mutable.Queue[(Int, Int)] = new mutable.Queue[(Int, Int)]()

      @scala.annotation.tailrec
      def findMaxDistance(ballIndex: Int, currMin: Int): Int  = {
        if(ballIndex > m) {
          currMin
        } else {
          val (low, high) = queue.dequeue()
          val currBallPosition = (low + high) / 2
          queue.enqueue((low, currBallPosition))
          queue.enqueue((currBallPosition, high))
          findMaxDistance(ballIndex + 1,
            Math.min(sortedPositions(high) - sortedPositions(currBallPosition),
              sortedPositions(currBallPosition) - sortedPositions(low)))
        }
      }

      queue.enqueue((0, sortedPositions.length - 1))
      findMaxDistance(3, sortedPositions(sortedPositions.length - 1) - sortedPositions.head)

    }
  }
}
