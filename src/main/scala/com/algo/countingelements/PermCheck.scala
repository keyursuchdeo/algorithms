package com.algo.countingelements

import scala.collection.mutable

object PermCheck extends App {
  val a = Array(4)
  val output = Solution.solution(a)
  println(output)
}

object Solution {
  var countMap: mutable.Map[Int, Int] = mutable.Map.empty
  def solution(a: Array[Int]): Int = {
    def elemCount(index: Int = 0, currMax: Int = 0): (Int, Int) = {
      if (index == a.length) {
        (1, currMax)
      } else {
        val newCount = countMap.get(a(index)).fold(1)(_ + 1)
        if (newCount > 1) {
          (0, currMax)
        } else {
          countMap.update(a(index), newCount)
          elemCount(index + 1, Math.max(a(index), currMax))
        }
      }
    }

    val (intermediateOutput, maxElement) = elemCount()
    calcOutput(intermediateOutput, maxElement)
  }

  private def calcOutput(intermediateOutput: Int, maxElement: Int): Int = {
    if(intermediateOutput == 0) {
      0
    } else {
      if ((1 to maxElement).forall(elem => countMap.contains(elem))) 1 else 0
    }
  }
}
