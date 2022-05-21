package com.algo.arrays

object MinimumNumberOfOperationsMoveAllBallsEachBox extends App {

  object Solution {
    def minOperations(boxes: String): Array[Int] = {
      val ballInBoxes = boxes.toCharArray
      val operations = new Array[Int](ballInBoxes.length)

      @scala.annotation.tailrec
      def findBoxesWithBalls(index: Int, boxesWithBalls: Seq[Int]): Seq[Int] = {
        if (index == ballInBoxes.length) {
          boxesWithBalls
        } else {
          if (ballInBoxes(index) == '1') {
            findBoxesWithBalls(index + 1, index +: boxesWithBalls)
          } else {
            findBoxesWithBalls(index + 1, boxesWithBalls)
          }
        }
      }


      @scala.annotation.tailrec
      def findMinOperations(index: Int, boxesWithBalls: Seq[Int]): Unit = {
        if (index == ballInBoxes.length) {
          ()
        } else {
          operations(index) =
            boxesWithBalls.map(box => {
              Math.abs(box - index)
            }).sum
          findMinOperations(index + 1, boxesWithBalls)
        }
      }

      findMinOperations(0, findBoxesWithBalls(0, Nil))
      operations
    }
  }

}
