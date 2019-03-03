package com.algo.z

object SmallestNumber extends App {
  val input = 1
  println(new Solution1().solution(input))
}


class Solution1 {
  def solution(n: Int): Int = {
    val numOfDigits = n.toString.length
    if (numOfDigits == 1) 0 else Math.pow(10, numOfDigits - 1).toInt
  }
}