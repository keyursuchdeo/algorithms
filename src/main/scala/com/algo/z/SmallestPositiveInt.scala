package com.algo.z

import scala.annotation.tailrec

object SmallestPositiveInt extends App {
  //val input = Array(1, 3, 6, 4, 1, 2)
  // val input = Array(1, 2, 3)
//  val input = Array(-1, -3)
//  println(new Solution().solution(input))
}

class Solution {
  def solution(a: Array[Int]): Int = {
    val inputSize = a.length
    val sortedInput = a.sorted
    val smallestPossible = 1

    @tailrec
    def find(index: Int = 0, smallest: Int = smallestPossible): Int = {
      if (index == inputSize) {
        smallest
      } else {
        if (sortedInput(index) == smallest) {
          find(index + 1, smallest + 1)
        } else {
          find(index + 1, smallest)
        }
      }
    }

    if(sortedInput(inputSize -1) <= 0) {
      smallestPossible
    } else {
      find()
    }
  }
}
