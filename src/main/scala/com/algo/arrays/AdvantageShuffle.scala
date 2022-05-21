package com.algo.arrays

object AdvantageShuffle extends App {
  object Solution {
    def advantageCount(A: Array[Int], B: Array[Int]): Array[Int] = {
      object IndexOrder extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = {
          B(y) compare B(x)
        }
      }

      val indicesB: Array[Int] = B.indices.toArray
      val sortedIndexB = indicesB.sorted(IndexOrder)

      val sortedA = A.sorted
      val ans = new Array[Int](B.length)

      @scala.annotation.tailrec
      def find(index: Int, low: Int, high: Int): Unit = {
        if(index == ans.length) {
          ()
        } else {
          if(sortedA(high) > B(sortedIndexB(index))) {
            ans(sortedIndexB(index)) = sortedA(high)
            find(index + 1, low, high - 1)
          } else {
            ans(sortedIndexB(index)) = sortedA(low)
            find(index + 1, low + 1, high)
          }
        }
      }

      find(0, 0, ans.length - 1)
      ans
    }
  }
}
