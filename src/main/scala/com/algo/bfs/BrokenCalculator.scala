package com.algo.bfs

import scala.collection.mutable

class BrokenCalculator {
  object Solution {
    def brokenCalc(X: Int, Y: Int): Int = {

      var visited = Set[Int]()

      @scala.annotation.tailrec
      def findFrom(queue: mutable.Queue[(Int, Int)]): Int = {
        if(queue.isEmpty) {
          -1
        } else {
          val (num, stepCount) = queue.dequeue()
          if (num == Y) {
            stepCount
          } else {
            if (num * 2 < 2 * Y && !visited.contains(num * 2)) {
              visited = visited + (num * 2)
              queue.enqueue((num * 2, stepCount + 1))
            } else {
              if (num - 1 > 0 && !visited.contains(num - 1)) {
                visited = visited + (num - 1)
                queue.enqueue((num - 1, stepCount + 1))
              }
              if (num < Y && !visited.contains(num * 2)) {
                visited = visited + (num * 2)
                queue.enqueue((num * 2, stepCount + 1))
              }
            }
            findFrom(queue)
          }
        }
      }

      if(X > Y) {
        X - Y
      } else {
        val q = new mutable.Queue[(Int, Int)]()
        q.enqueue((X, 0))
        findFrom(q)
      }
    }
  }
}
