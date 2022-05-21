package com.algo.aws

object SmallestPositiveInteger extends App {
  //val input = Array(1, 3, 6, 4, 1, 2)
  //val input = Array(1, 2, 3)
  val input = Array(-1, -3)
  val output = new Solution().solution(input)
  println(output)
}

class Solution {
  def solution(a: Array[Int]): Int = {
    val sortedA = a.sorted

    def find(index: Int = 0, smallestInt: Int = 1): Int = {
      if(index == a.length) {
        smallestInt
      } else {
        if (sortedA(index) == smallestInt) {
          find(index + 1, smallestInt + 1)
        } else {
          find(index + 1, smallestInt)
        }
      }
    }

    find()
  }
}
