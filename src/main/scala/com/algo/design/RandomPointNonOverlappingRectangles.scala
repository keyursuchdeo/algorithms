package com.algo.design

import scala.util.Random

object RandomPointNonOverlappingRectangles extends App {

  val r = Array(Array(-2,-2,-1,-1), Array(1,0,3,0))
  val res = new Solution(r)
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))
  println(res.pick().mkString(","))

  class Solution(_rects: Array[Array[Int]]) {

    private val pointCount: Array[Int] = _rects.map(countPoints).scanLeft(0)(_ + _).tail
    private val totalPoints: Int = pointCount.reverse.head

    private def countPoints(rectangle: Array[Int]) = {
      val Array(blX, blY, trX, trY) = rectangle
      val width = trX - blX
      val length = trY - blY
      (length + 1) * (width + 1)
    }

    private def findRectangle(point: Int): Int = {
      @scala.annotation.tailrec
      def find(low: Int, high: Int, output: Int): Int = {
        if(high < low) {
          output
        } else {
          val mid = (low + high) / 2
          if(pointCount(mid) < point) {
            find(mid + 1, high, output)
          } else if (pointCount(mid) > point) {
            find(low, mid - 1, mid)
          } else {
            mid
          }
        }
      }
      find(0, pointCount.length - 1, 0)
    }

    private def pickPoint(point: Int, rIndex: Int): Array[Int] = {
      val pointInRect = if (rIndex == 0) point else point - pointCount(rIndex - 1)
      val Array(blX, blY, trX, trY) = _rects(rIndex)
      val width = trX - blX + 1
      val pointWidth = (pointInRect - 1) % width
      val pointHeight = (pointInRect - 1) / width
      Array(blX + pointWidth, blY + pointHeight)
    }

    def pick(): Array[Int] = {
      val point = Random.nextInt(totalPoints) + 1
      val rIndex = findRectangle(point)
      println(s"$point, $rIndex")
      pickPoint(point, rIndex)
    }

  }


}
