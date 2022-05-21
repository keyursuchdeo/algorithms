package com.algo.arrays

object RemoveCoveredIntervals extends App {
  object Solution {
    def removeCoveredIntervals(intervals: Array[Array[Int]]): Int = {
      object IntervalOrder extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(x1, x2) = x
          val Array(y1, y2) = y

          if(x1 == y1) {
            y2 compare x2
          } else {
            y1 compare x1
          }
        }
      }

      @scala.annotation.tailrec
      def remove(index: Int, prev: Array[Int], count: Int, sortedIntervals: Array[Array[Int]]): Int = {
        if(index == sortedIntervals.length) {
          count
        } else {
          val Array(p1, p2) = prev
          val Array(c1, c2) = sortedIntervals(index)

          if(p1 == c1) {
            remove(index + 1, sortedIntervals(index), count, sortedIntervals)
          } else {
            if(p2 >= c2) {
              remove(index + 1, prev, count, sortedIntervals)
            } else {
              remove(index + 1, sortedIntervals(index), count + 1, sortedIntervals)
            }
          }
        }
      }

      if(intervals.length < 1) intervals.length else {
        val sortedIntervals: Array[Array[Int]] = intervals.sorted(IntervalOrder)
        remove(1, sortedIntervals.head, 1, sortedIntervals)
      }
    }
  }
}
