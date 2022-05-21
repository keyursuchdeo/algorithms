package com.algo.heap

object FurthestBuildingYouCanReach extends App {

  val a = Array(1,5,1,2,3,4,10000)
  val b = 4
  val l = 1

  val res = Solution.furthestBuilding(a, b, l)
  println(res)

  object Solution {
    def furthestBuilding(heights: Array[Int], bricks: Int, ladders: Int): Int = {
      object MinOrder extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = {
          y compare x
        }
      }

      val minHeap = scala.collection.mutable.PriorityQueue.empty(MinOrder)

      @scala.annotation.tailrec
      def move(index: Int, remainingBricks: Int, remainingLadders: Int): Int = {
        if(index + 1 == heights.length) {
          index
        } else {
          val diff = heights(index + 1) - heights(index)
          if(diff <= 0) {
            move(index + 1, remainingBricks, remainingLadders)
          } else {
            if(remainingLadders > 0) {
              minHeap.enqueue(diff)
              move(index + 1, remainingBricks, remainingLadders - 1)
            } else {
              minHeap.headOption match {
                case Some(value) if value < diff && remainingBricks >= value =>
                  minHeap.enqueue(diff)
                  move(index + 1, remainingBricks - minHeap.dequeue(), remainingLadders)
                case _ if remainingBricks >= diff =>
                  move(index + 1, remainingBricks - diff, remainingLadders)
                case _ =>
                  index
              }
            }
          }
        }
      }

      move(0, bricks, ladders)
    }
  }
}
