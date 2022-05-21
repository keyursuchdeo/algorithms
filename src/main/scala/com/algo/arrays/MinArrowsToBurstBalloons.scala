package com.algo.arrays

object MinArrowsToBurstBalloons extends App {
  object Solution {
    def findMinArrowShots(points: Array[Array[Int]]): Int = {

      object BalloonOrder extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(x1, x2) = x
          val Array(y1, y2) = y

          if (x1 == y1) {
            x2 compare y2
          } else {
            x1 compare y1
          }
        }
      }


      val sortedBalloons: Array[Array[Int]] = points.sorted(BalloonOrder)

      def isOverlapping(prev: Array[Int], curr: Array[Int]): Boolean = {
        val Array(p1, p2) = prev
        val Array(c1, c2) = curr
        if(p1 == c1) {
          true
        } else {
          if(p2 < c1) {
            false
          } else {
            true
          }
        }
      }

      @scala.annotation.tailrec
      def countArrows1(index: Int, maxMin: Int, minMax: Int, count: Int): Int = {
        if (index == sortedBalloons.length) {
          count
        } else if (index == 0) {
          countArrows1(index + 1, sortedBalloons(index)(0), sortedBalloons(index)(1), count + 1)
        } else {
          if (isOverlapping(sortedBalloons(index - 1), sortedBalloons(index))) {
            countArrows1(index + 1, Math.max(maxMin, sortedBalloons(index)(0)), Math.min(minMax, sortedBalloons(index)(1)), count)
          } else {
            countArrows1(index + 1, sortedBalloons(index)(0), sortedBalloons(index)(1), count + 1)
          }
        }
      }

//      @scala.annotation.tailrec
//      def countArrows(index: Int, prev: Array[Int], count: Int): Int = {
//        if (index == sortedBalloons.length) {
//          count
//        } else if (index == 0) {
//          countArrows(index + 1, sortedBalloons(index), count + 1)
//        } else {
//          println(prev.mkString(","))
//          val Array(p1, p2) = prev
//          val Array(c1, c2) = sortedBalloons(index)
//
//          if (p1 == c1) {
//            if (p2 <= c2) {
//              countArrows(index + 1, Array(p1, c2), count)
//            } else {
//              countArrows(index + 1, prev, count)
//            }
//          } else {
//            if (p2 < c1) {
//              countArrows(index + 1, Array(c1, c2), count + 1)
//            } else if (p2 == c1) {
//              countArrows(index + 1, Array(p1, c2), count)
//            } else {
//              if (p2 > c2) {
//                countArrows(index + 1, prev, count)
//              } else {
//                countArrows(index + 1, Array(p1, c2), count)
//              }
//            }
//          }
//        }
//      }

      println(sortedBalloons.map(_.mkString(",")).mkString("|"))
      countArrows1(0, -1, -1, 0)

    }
  }
}
