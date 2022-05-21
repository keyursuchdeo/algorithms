package com.algo.arrays

object InsertInterval extends App {

//  val a = Array(Array(1, 2), Array(3, 5), Array(6, 7), Array(8, 10), Array(12, 16))
//  val n = Array(4, 8)

  val a = Array(Array(1, 3), Array(6, 9))
  val n = Array(2, 5)
  val res = Solution.insert(a, n)
  res.foreach(i => println(i.mkString(",")))

  object Solution {
    def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
      def compareArrays(a1: Array[Int], a2: Array[Int]) = {
        if (a1.sameElements(a2)) {
          0
        } else {
          if (a1.head == a2.head) {
            if(a1(1) < a2(1)) {
              -1
            } else {
              1
            }
          }  else if (a1.head < a2.head){
            -1
          } else {
            1
          }
        }
      }

      def insertAt(index: Int): Array[Array[Int]] = {
        val before = intervals.take(index)
        val after = intervals.drop(index)
        before ++ (newInterval +: after)
      }

      @scala.annotation.tailrec
      def insertInterval(low: Int, high: Int): Array[Array[Int]] = {
        if(high < low) {
          insertAt(low)
        } else {
          val mid = (low + high) / 2
          val output = compareArrays(intervals(mid), newInterval)
          if(output == 0) {
            intervals
          } else if (output == -1) {
            insertInterval(mid + 1, high)
          } else {
            insertInterval(low, mid - 1)
          }
        }
      }

      @scala.annotation.tailrec
      def mergeSortedIntervals(index: Int, updatedIntervals: Array[Array[Int]], mergedIntervals: Seq[Array[Int]]): Seq[Array[Int]] = {
        if(index == updatedIntervals.length) {
          mergedIntervals
        } else if (index == 0) {
          mergeSortedIntervals(index + 1, updatedIntervals, updatedIntervals(0) +: mergedIntervals)
        } else {
          val Array(hLow, hHigh) = mergedIntervals.head
          val Array(cLow, cHigh) = updatedIntervals(index)
          if(hLow == cLow) {
            mergeSortedIntervals(index + 1, updatedIntervals, Array(hLow, Math.max(hHigh, cHigh)) +: mergedIntervals.tail)
          } else if (cLow <= hHigh) {
            if(cHigh <= hHigh) {
              mergeSortedIntervals(index + 1, updatedIntervals, mergedIntervals)
            } else {
              mergeSortedIntervals(index + 1, updatedIntervals, Array(hLow, cHigh) +: mergedIntervals.tail)
            }
          } else { //shouldn't happen as list if sorted
            mergeSortedIntervals(index + 1, updatedIntervals, updatedIntervals(index) +: mergedIntervals)
          }
        }
      }

      val updatedIntervals = insertInterval(0, intervals.length - 1)
      updatedIntervals.foreach(i => println(i.mkString(",")))
      println("---------")
      mergeSortedIntervals(0, updatedIntervals, Nil).reverse.toArray
    }
  }
}
