package com.algo.aws

object Siblings extends App {
  val input = 12
  val output = new SolutionSib().solution(input)
  println(output)
}

class SolutionSib {
  def solution(n: Int): Int = {
    if (n < 11) {
      n
    } else {
      findLargestNumber(n)
    }
  }

  def findLargestNumber(n: Int): Int = {
    val sortedChars: String = n.toString.sorted.reverse
    sortedChars.toInt
  }
}
