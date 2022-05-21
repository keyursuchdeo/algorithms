package com.algo.arrays

object MergeIntervals extends App {
  object Solution {
    def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {

      implicit object MinOrdering extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val a = x.head compare y.head
          if (a == 0) {
            x(1) compare y(1)
          } else {
            a
          }
        }
      }

      lazy val sortedIntervals: Array[Array[Int]] = intervals.sorted(MinOrdering)

      @scala.annotation.tailrec
      def mergeSortedIntervals(index: Int, mergedIntervals: Seq[Array[Int]]): Seq[Array[Int]] = {
        if(index == intervals.length) {
          mergedIntervals
        } else if (index == 0) {
          mergeSortedIntervals(index + 1, sortedIntervals(0) +: mergedIntervals)
        } else {
          val Array(hLow, hHigh) = mergedIntervals.head
          val Array(cLow, cHigh) = sortedIntervals(index)
          if(hLow == cLow) {
            mergeSortedIntervals(index + 1, Array(hLow, Math.max(hHigh, cHigh)) +: mergedIntervals.tail)
          } else if (cLow < hHigh) {
              if(cHigh <= hHigh) {
                mergeSortedIntervals(index + 1, mergedIntervals)
              } else {
                mergeSortedIntervals(index + 1, Array(hLow, cHigh) +: mergedIntervals.tail)
              }
          } else { //shouldn't happen as list if sorted
            mergeSortedIntervals(index + 1, sortedIntervals(index) +: mergedIntervals)
          }
        }
      }

      mergeSortedIntervals(0, Nil).toArray

    }
  }
}
