package com.algo.arrays

object ValidSquare2 extends App {

  val res = Solution.validSquare(Array(0, 0), Array(1, 1), Array(1, 0), Array(0, 1))
  println(res)

  object Solution {
    def validSquare(p1: Array[Int], p2: Array[Int], p3: Array[Int], p4: Array[Int]): Boolean = {
      def distanceBetweenPoints(point1: Array[Int], point2: Array[Int]): Double = {
        val Array(x1, y1) = point1
        val Array(x2, y2) = point2
        val deltaX = x2 - x1
        val deltaY = y2 - y1

        Math.sqrt(
          deltaX * deltaX + deltaY * deltaY
        )

      }

      def getNeighbours(point: Array[Int], remainingPoints: Array[Array[Int]]): Option[(Array[Array[Int]], Array[Int])] = {
        val Array(rp1, rp2, rp3) = remainingPoints
        val d1 = distanceBetweenPoints(point, rp1)
        val d2 = distanceBetweenPoints(point, rp2)
        if (d1 == d2) {
          Option((Array(rp1, rp2), rp3))
        } else {
          val d3 = distanceBetweenPoints(point, rp3)
          if (d1 == d3) {
            Option((Array(rp1, rp3), rp2))
          } else if (d2 == d3) {
            Option((Array(rp2, rp3), rp1))
          } else {
            None
          }
        }
      }

      def areLinesPerpendicular(point1: Array[Int], point2: Array[Int], point3: Array[Int]): Boolean = {
        val Array(px1, py1) = point1
        val Array(px2, py2) = point2
        val Array(px3, py3) = point3

        if(px2 - px1 == 0) {
          (py3 - py1) / (px3 - px1) == 0
        } else if (px3 - px1 == 0) {
          (py2 - py1) / (px2 - px1) == 0
        } else {
          val m1 = (py2 - py1) / (px2 - px1).toDouble
          val m2 = (py3 - py1) / (px3 - px1).toDouble
          m1 * m2 == -1
        }
      }

      getNeighbours(p1, Array(p2, p3, p4)) match {
        case None => false
        case Some((Array(n1, n2), n3)) =>
          distanceBetweenPoints(n1, n3) == distanceBetweenPoints(n2, n3) &&
            areLinesPerpendicular(p1, n1, n2) &&
            areLinesPerpendicular(n3, n1, n2) &&
            areLinesPerpendicular(n1, p1, n3) &&
            areLinesPerpendicular(n2, p1, n3)
      }
    }
  }

}
