package com.algo.timecomplexity

object MissingElem extends App {
  println(new Solution1().solution(Array(2, 3, 1, 5)))
}

class Solution1 {
  def solution(a: Array[Int]): Int = {
    val sortedArray = a.sorted
    sortedArray.zipWithIndex.find(input => {
      val (num, index) = input
      num != index + 1
    }).get._2 + 1
  }
}

