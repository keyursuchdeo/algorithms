package com.algo.dp

object UniqueBinarySearchTrees extends App {
  val res = Solution.numTrees(19)
  println(res)

  object Solution {
    def numTrees(n: Int): Int = {
      val a = new Array[Int](n)
      def count(num: Int): Int = {
        if (num == 1) {
          1
        } else if (a(num - 1) > 0) {
          a(num - 1)
        }else {
          var sum: Int = 0
          for (i <- 1 to num) yield {
            val a = if (i == 1 || i == num) {
              count(num - 1)
            } else {
              count(i - 1) * count(num - i)
            }
            sum = sum + a
          }
          a(num - 1) = sum
          sum
        }
      }
      count(n)
    }
  }
}
