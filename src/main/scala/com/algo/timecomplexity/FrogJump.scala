package com.algo.timecomplexity

object FrogJump extends App {
  println(new Solution().solution(10, 70, 30))
}

class Solution {
  def solution(x: Int, y: Int, d: Int): Int = {
    Math.ceil((y - x) / d.toDouble).toInt
  }
}
