package com.algo.countingelements

object FrogRiver extends App {
  val input = Array(0)
  val output = Solution1.solution(0, input)
  println(output)
}

object Solution1 {
  def solution(x: Int, leafPositions: Array[Int]): Int = {
    val countArray = Array.fill(x + 1)(0)
    countArray(0) = 1
    println(countArray.mkString(","))

    def lastLeafFallIndex(index: Int = 0, leafFallIndex: Int = 0): Int = {
      if(index == leafPositions.length) {
        leafFallIndex
      } else {
        if(countArray(leafPositions(index)) == 0) {
          countArray(leafPositions(index)) +=1
          lastLeafFallIndex(index + 1, index)
        } else {
          lastLeafFallIndex(index + 1, leafFallIndex)
        }
      }
    }

    val leafFallIndex = lastLeafFallIndex()
    println(countArray.mkString(","))
    println(leafFallIndex)
    if (countArray.forall(_ == 1)) leafFallIndex else -1
  }
}