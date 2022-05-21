package com.algo.arrays

import scala.collection.mutable.ArrayBuffer

object MyCalendarI extends App {
  class MyCalendar() {
    private var sortedSchedules: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()

    def add(schedule: (Int, Int)): Boolean = {
      val (sStart, sEnd) = schedule
      @scala.annotation.tailrec
      def findPos(low: Int, high: Int, pos: Int): Int = {
        if(high < low) {
          pos
        } else {
          val mid = (high + low) / 2
          val (start, _) = sortedSchedules(mid)
          if(start == sEnd) {
            pos
          } else if (start > sEnd) {
            findPos(low, mid - 1, mid)
          } else {
            if(start < sStart) {
              findPos(mid + 1, high, mid)
            } else {
              pos
            }
          }
        }
      }

      val foundPos = findPos(0, sortedSchedules.length, -1)
      if(foundPos == -1) {
        false
      } else {
        val (before, after) = sortedSchedules.splitAt(foundPos)
        sortedSchedules = before ++ (schedule +: after)
        true
      }

    }

    def book(start: Int, end: Int): Boolean = {
      if(sortedSchedules.isEmpty) {
        sortedSchedules = (start, end) +: sortedSchedules
        true
      } else {
        add((start, end))
      }
    }

  }

  /**
   * Your MyCalendar object will be instantiated and called as such:
   * var obj = new MyCalendar()
   * var param_1 = obj.book(start,end)
   */
}
