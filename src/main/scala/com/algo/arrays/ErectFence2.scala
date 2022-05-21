package com.algo.arrays

object ErectFence2 extends App {

  val trees = Array(Array(3,7),Array(6,8),Array(7,8),Array(11,10),Array(4,3),Array(8,5),Array(7,13),Array(4,13))
  val res = Solution.outerTrees(trees)
  println(res.map(_.mkString(",")).mkString("|"))

  object Solution {
    def outerTrees(trees: Array[Array[Int]]): Array[Array[Int]] = {

      def findLowestPoint(index: Int, lowest: Array[Int]): Array[Int] = {
        if (index == trees.length) {
          lowest
        } else {
          val Array(x, y) = trees(index)
          val Array(lx, ly) = lowest
          if (y < ly) {
            findLowestPoint(index + 1, trees(index))
          } else if (y == ly) {
            if (x < lx) {
              findLowestPoint(index + 1, trees(index))
            }
          }
          findLowestPoint(index + 1, lowest)
        }
      }

      val ref = findLowestPoint(index = 1, trees.head)


      object AngleSort extends Ordering[Array[Int]] {
        override def compare(p1: Array[Int], p2: Array[Int]): Int = {
          val Array(x1, y1) = p1
          val Array(x2, y2) = p2

          Math.atan()
        }
      }
    }
  }
}
