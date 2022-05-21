package com.algo.bfs

import scala.collection.mutable

object MinimumOperationsToReduceXToZero extends App {
  object Solution {
    def minOperations(nums: Array[Int], x: Int): Int = {
      @scala.annotation.tailrec
      def find(queue: mutable.Queue[(Int, Int, Int, Int)]): Int = {
        if(queue.isEmpty) {
          -1
        } else {
          val (min, max, remainingX, opCount) = queue.dequeue()
          if(max < min) {
            find(queue)
          } else if (max == min) {
            if(remainingX - nums(min) == 0) {
              1 + opCount
            } else  {
              find(queue)
            }
          } else {
            if(remainingX - nums(min) == 0 || remainingX - nums(max) == 0) {
              1 + opCount
            } else {
              if(nums(min) < remainingX && min + 1 < nums.length) {
                queue.enqueue((min + 1, max, remainingX - nums(min), opCount + 1))
              }
              if(nums(max) < remainingX && max - 1 >= 0) {
                queue.enqueue((min, max - 1, remainingX - nums(max), opCount + 1))
              }
              find(queue)
            }
          }
        }
      }

      val queue = mutable.Queue[(Int, Int, Int, Int)]()
      queue.enqueue((0, nums.length - 1, x, 0))
      find(queue)
    }
  }
}
