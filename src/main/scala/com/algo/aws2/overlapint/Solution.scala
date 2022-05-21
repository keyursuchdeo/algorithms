package com.algo.aws2.overlapint

class Solution {
  def merge(intervals: Seq[Interval]): Seq[Interval] = {
    val sortedIntervals = intervals.sortBy(interval => interval.start + interval.end)

    @scala.annotation.tailrec
    def mergeIntervals(mergedIntervals: Seq[Interval], sortedIntervals: Seq[Interval]): Seq[Interval] = {
      sortedIntervals match {
        case Nil => mergedIntervals
        case x :: xs =>
          if(overlappingIntervals(mergedIntervals.head, x)) {
            mergeIntervals(mergeAndPrependOverlappingIntervals(mergedIntervals.head, x, mergedIntervals.tail), xs)
          } else {
            mergeIntervals(prependInterval(x, mergedIntervals), xs)
          }
      }
    }

    val mergedIntervals = mergeIntervals(Seq(sortedIntervals.head), sortedIntervals.tail)

    mergedIntervals.sortBy(interval => interval.start + interval.end)
  }

  def overlappingIntervals(int1: Interval, int2: Interval): Boolean =
    int1.isOverlapping(int2)

  def mergeAndPrependOverlappingIntervals(int1: Interval, int2: Interval, mergedIntervals: Seq[Interval]): Seq[Interval] =
    prependInterval(mergeOverlappingIntervals(int1, int2), mergedIntervals)

  def prependInterval(interval: Interval, intervals: Seq[Interval]): Seq[Interval] =
    interval +: intervals

  def mergeOverlappingIntervals(int1: Interval, int2: Interval): Interval =
    int1.mergeOverlappingInterval(int2)
}

object MainOverlap extends App {
  val sol = new Solution()
  val a = Seq(Interval(1, 4), Interval(4, 8), Interval(8, 18), Interval(15, 18))
  val mergedA = sol.merge(a)
  println(mergedA)
}
