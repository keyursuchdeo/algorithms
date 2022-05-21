package com.algo.aws2.overlapint

case class Interval(start: Int = 0, end: Int = 0) {
  def isOverlapping(interval: Interval): Boolean =
    !(start > interval.end || end < interval.start)

  def mergeOverlappingInterval(interval: Interval): Interval =
    Interval(Math.min(start, interval.start), Math.max(end, interval.end))
}
