package com.algo.aws3

import scala.collection.JavaConverters._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

//object Solution {
//  def solution(a: Array[Int]): Int = {
//    val occurrences: Array[Int] = Array.ofDim[Int](a.length)
//    val occCounter: Int => Unit = countOccurences(occurrences)
//    a.foreach(element => occCounter(element))
//    val missing: Option[(Int, Int)] = occurrences.zipWithIndex.find(isMissing)
//    missing.map(_._2 + 1).getOrElse(a.length + 1)
//  }
//
//  private def countOccurences(occurrences: Array[Int])(element: Int): Array[Int] = {
//    if (element > 0 && element < occurrences.length + 1) {
//      occurrences(element - 1) = occurrences(element - 1) + 1
//    }
//    occurrences
//  }
//
//  private def isMissing(countAndIndex: (Int, Int)): Boolean = {
//    val (count, _) = countAndIndex
//   count == 0
//  }
//
//}

//object SmallestMissingPositiveInt extends App {
//  val a = Array(1, 3, 6, 4, 1, 2)
////  val a = Array(1, 2, 3)
////  val a = Array(-1, -3)
//  val out = Solution.solution(a)
//  println(out)
//}
