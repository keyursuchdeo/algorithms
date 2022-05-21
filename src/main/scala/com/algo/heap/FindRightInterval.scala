package com.algo.heap

import scala.collection.mutable

object FindRightInterval extends App {
  object Solution {
    def findRightInterval(intervals: Array[Array[Int]]): Array[Int] = {

      object IntervalOrder extends Ordering[(Array[Int], Int)] {
        def compare(x: (Array[Int], Int), y: (Array[Int], Int)): Int = {
          val (Array(x1, x2), _) = x
          val (Array(y1, y2), _) = y
          if(x1 == y1) {
            y2 compare x2
          } else {
            y1 compare x1
          }
        }
      }

      val intervalsWithIndex: Array[(Array[Int], Int)] = intervals.zipWithIndex
      val sortedIntervalsWithIndex: Array[(Array[Int], Int)] = intervalsWithIndex.sorted(IntervalOrder)
      val output = Array.fill[Int](intervals.length)(-1)


      @scala.annotation.tailrec
      def find(num: Int, low: Int, high: Int, foundIndex: Int): Int = {
        if(high < low) {
          foundIndex
        } else {
          val mid = (low + high) / 2
          if(sortedIntervalsWithIndex(mid)._1.head >= num) {
            find(num, low, mid - 1, sortedIntervalsWithIndex(mid)._2)
          } else {
            find(num, mid + 1, high, foundIndex)
          }
        }
      }

      @scala.annotation.tailrec
      def fillOutput(index: Int): Unit = {
        if(index == intervals.length) {
          ()
        } else {
          val rightIndex = find(sortedIntervalsWithIndex(index)._1(2), index + 1, intervals.length - 1, -1)
          output(sortedIntervalsWithIndex(index)._2) = rightIndex
          fillOutput(index + 1)
        }
      }

      fillOutput(0)
      output
    }
  }
}
