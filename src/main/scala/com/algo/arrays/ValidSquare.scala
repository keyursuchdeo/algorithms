package com.algo.arrays

object ValidSquare extends App {
  object Solution {
    def validSquare(p1: Array[Int], p2: Array[Int], p3: Array[Int], p4: Array[Int]): Boolean = {
      object SquarePoints extends Ordering[Array[Int]] {
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

      def check(sp1: Array[Int], sp2: Array[Int], sp3: Array[Int], sp4: Array[Int]): Boolean = {
        val Array(sp1x, sp1y) = sp1
        val Array(sp2x, sp2y) = sp2
        val Array(sp3x, sp3y) = sp3
        val Array(sp4x, sp4y) = sp4

        sp1x == sp2x && sp2y == sp4y && sp3x == sp4x &&
          (sp2y - sp1y) == (sp4x - sp2x) &&
          (sp4x - sp2x) == (sp4y - sp3y) &&
          (sp4y - sp3y) == (sp3x - sp1x)

      }

      val Array(sp1, sp2, sp3, sp4) = Array(p1, p2, p3, p4).sorted(SquarePoints)
      check(sp1, sp2, sp3, sp4)
    }
  }
}
