package com.algo.arrays

import scala.annotation.tailrec
import scala.collection.mutable

object OddOccurrencesInArray extends App {
  val input = Array(9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 9, 3, 9, 3, 9, 7, 9, 7, 7, 7, 11)
  // val input = Array(2, 2, 4)
  println(new Solution().solution(input))
}

class Solution {
  def solution(a: Array[Int]): Int = {
    println(a.length)
    var numCount = mutable.Map.empty[Int, Int]
    def mapNum(elem: Int)= {
      val updatedVal = numCount.get(elem).map(_ + 1).getOrElse(1)
      numCount += elem -> updatedVal
    }
    a.foreach(mapNum)
    println(numCount)
    numCount.find(_._2 == 1).map(_._1).get
  }


}
