package com.algo.heap

object ConstructTargetArrayWithMultipleSums extends App {
  object Solution {
    def isPossible(target: Array[Int]): Boolean = {
      object MaxOrder extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = {
          x compare y
        }
      }
      val maxHeap = scala.collection.mutable.PriorityQueue.empty(MaxOrder)
      target.foreach(element => maxHeap.enqueue(element))

      @scala.annotation.tailrec
      def check(currSum: Int): Boolean = {
          val maxElement = maxHeap.dequeue()
          if(maxElement == 1) {
            true
          } else {
            val toSub = currSum - maxElement
            if (maxElement <= toSub || toSub < 1) {
              false
            } else {
              val diff = maxElement % toSub
              maxHeap.enqueue(diff)
              check(toSub + diff)
            }
          }
      }

      check(target.sum)
    }
  }
}
