package com.algo.arrays

object ContainerMostWater extends App {
  val h = Array(1, 8, 6, 2, 5, 4, 8, 3, 7)
  val res = Solution.maxArea(h)
  println(res)

  object Solution {
    def maxArea(height: Array[Int]): Int = {

      @scala.annotation.tailrec
      def find(low: Int, high: Int, maxArea: Int): Int = {
        if (high <= low) {
          maxArea
        } else {
          if (height(low) < height(high)) {
            find(low + 1, high, Math.max(maxArea, (high - low) * height(low)))
          } else {
            find(low, high - 1, Math.max(maxArea, (high - low) * height(high)))
          }
        }
      }

      find(0, height.length - 1, 0)

    }
  }

}
