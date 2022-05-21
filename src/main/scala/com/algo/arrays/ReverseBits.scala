package com.algo.arrays

object ReverseBits extends App {

  val res = Solution.reverseBits(43261596)
  println(res)

  object Solution {
    // you need treat n as an unsigned value
    def reverseBits(x: Int): Int = {
      @scala.annotation.tailrec
      def reverse(index: Int, currX: Int, ans: Int): Int = {
        if(index == 32) {
          ans
        } else {
          reverse(index + 1, currX >> 1, (ans << 1)^(currX & 1))
        }
      }

      reverse(0, x, 0)
    }
  }
}
