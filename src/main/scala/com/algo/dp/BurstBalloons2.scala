package com.algo.dp

import scala.collection.mutable
import scala.util.Try

object BurstBalloons2 extends App {

  val n = Array(3, 1, 5, 8)
  val res = Solution.maxCoins(n)
  println(res)

  object Solution {
    def maxCoins(nums: Array[Int]): Int = {

      val coins = Array.ofDim[Int](nums.length, nums.length)

      @scala.annotation.tailrec
      def calculate(winSize: Int): Unit = {
        if(winSize > nums.length) {
          ()
        } else {
          for(
            row <- nums.indices;
            col <- row until row + winSize;
            if col < nums.length && col - row == winSize - 1
          ) {

          }
          calculate(winSize + 1)
        }
      }
      calculate(1)
      1

    }
  }
}
