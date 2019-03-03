package com.algo.arrays

import scala.annotation.tailrec
import scala.collection.mutable

object OddOccurrencesInArray extends App {
  val input = Array(9, 3, 9, 3, 9, 7, 9)
  // val input = Array(2, 2, 4)
  println(new Solution().solution(input))
}

class Solution {
  def solution(a: Array[Int]): Int = {
    val inputSize = a.length

    @tailrec
    def prepNumCount(index: Int = 0, map: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]): mutable.Map[Int, Int] = {
      if (index == inputSize) {
        map
      } else {
        prepNumCount(index + 1, mapNum(a(index), map))
      }
    }

    val updatedNumCount: mutable.Map[Int, Int] = prepNumCount()
    updatedNumCount.find(_._2 == 1).map(_._1).get
  }

  private def mapNum(elem: Int, numCount: mutable.Map[Int, Int]): mutable.Map[Int, Int] = {
    val updatedVal = numCount.get(elem).map(_ + 1).getOrElse(1)
    numCount + (elem -> updatedVal)
  }
}
