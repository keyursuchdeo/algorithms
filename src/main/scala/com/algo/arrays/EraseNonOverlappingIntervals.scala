package com.algo.arrays

object EraseNonOverlappingIntervals extends App {
  object Solution {
    def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int = {

      object IntervalOrdering extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(x1, x2) = x
          val Array(y1, y2) = y

          if(x1 == y1) {
            x2 compare y2
          } else {
            x1 compare y1
          }
        }
      }



      lazy val sortedIntervals = intervals.sorted(IntervalOrdering)

      @scala.annotation.tailrec
      def dropOverlapping(index: Int, prevInterval: Array[Int], count: Int): Int = {
        if(index == intervals.length) {
          count
        } else {
          val Array(p1, p2) = prevInterval
          val Array(c1, c2) = sortedIntervals(index)
          if(p1 == c1) {
            dropOverlapping(index + 1, prevInterval, count + 1)
          } else {
            if(c2 <= p2) {
              dropOverlapping(index + 1, Array(c1, c2), count + 1)
            } else {
              if(c1 >= p2) {
                dropOverlapping(index + 1, Array(c1, c2), count)
              } else {
                dropOverlapping(index + 1, prevInterval, count + 1)
              }
            }
          }
        }
      }

      if(intervals.length <= 1) {
        0
      } else {
        dropOverlapping(1, sortedIntervals.head, 0)
      }
    }
  }
}
