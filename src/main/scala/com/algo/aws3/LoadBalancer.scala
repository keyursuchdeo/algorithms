package com.algo.aws3

import scala.collection.JavaConverters._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution {
//  def solution(a: Array[Int]): Boolean = {
//    val totalTimeOfAllReqs = a.sum
//    val totalDropModulo3 = sumOfDropTimesModulo3(totalTimeOfAllReqs)
//    val occ = countByValue(a)
////    val droppableCombinations =
//
//
//  }

//  private def dropMatch(occ: Array[Int], possibleDropElem: Int, totalDropModulo3: Int): Int = {
//    if (possibleDropElem % 3 == 0 && totalDropModulo3 == 0) {
//
//    } else if (possibleDropElem % 3 == 1) {
//
//    } else {
//
//    }
//  }

  private def sumOfDropTimesModulo3(totalTime: Int): Int = {
    totalTime % 3
  }

  private def countByValue(a: Array[Int]): Array[Int] = {
    val occurrences: Array[Int] = Array.ofDim[Int](a.length)
    val occCounter: Int => Unit = countOccurrences(occurrences)
    a.foreach(element => occCounter(element))
    occurrences
  }

  private def countOccurrences(occurrences: Array[Int])(element: Int): Array[Int] = {
    occurrences(element - 1) = occurrences(element - 1) + 1
    occurrences
  }

}



object LoadBalancer extends App {

}
