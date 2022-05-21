package com.algo.heap

object MinimizeDeviationArray extends App {

  val a = Array(1,2)
  val res = Solution.minimumDeviation(a)
  println(res)

  object Solution {
    def minimumDeviation(nums: Array[Int]): Int = {

      object MaxOrder extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = x compare y
      }

      val maxHeap = scala.collection.mutable.PriorityQueue.empty(MaxOrder)

      @scala.annotation.tailrec
      def fillQueueAndFindMin(index: Int, min: Int): Int = {
        if(index == nums.length) {
          min
        } else {
          if(nums(index) % 2 == 0) {
            maxHeap.enqueue(nums(index))
            fillQueueAndFindMin(index + 1, Math.min(min, nums(index)))
          } else {
            maxHeap.enqueue(nums(index) * 2)
            fillQueueAndFindMin(index + 1, Math.min(min, nums(index) * 2))
          }
        }
      }

      @scala.annotation.tailrec
      def findDeviation(currDev: Int, currMin: Int): Int = {
        if(maxHeap.isEmpty) {
          currDev
        } else {
          val max = maxHeap.dequeue()
          if(max % 2 == 1) {
            currDev
          } else {
            maxHeap.enqueue(max / 2)
            findDeviation(Math.min(currDev, max - currMin), Math.min(currMin, max / 2))
          }
        }
      }

      val min = fillQueueAndFindMin(0, Int.MaxValue)
      findDeviation(Int.MaxValue, min)
    }

  }
}
