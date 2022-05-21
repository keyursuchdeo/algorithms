package com.algo.dp

import scala.collection.mutable

object DistinctSubsequences extends App {

    val s = "rrabbbit"
    val t = "rabbit"
//  val s = "babgbag"
//  val t = "bag"
  //  val s = "bccbcdcabadabddbccaddcbabbaaacdba"
  //  val t = "bccbbdc"
  val res = Solution.numDistinct(s, t)
  println(res)

  object Solution {
    def numDistinct(s: String, t: String): Int = {
      var map: mutable.Map[String, Int] = mutable.Map[String, Int]()

      def num(sIndex: Int, updatedS: String, count: Int): Int = {
        if (sIndex == updatedS.length || updatedS.length < t.length) {
          count
        } else {
          val (before, after) = updatedS.splitAt(sIndex)
          val newUpdatedS = s"$before${after.tail}"
          val updatedCount =
            if (newUpdatedS == t) {
              count + 1
            } else {
              count
            }
          val c = num(sIndex + 1, updatedS, updatedCount) + num(sIndex, newUpdatedS, 0)
          println(s"$updatedS -> $c")
          map = map + (updatedS -> c)
          c
        }
      }

      val out = if (s == t) 1 else num(0, s, 0)
      println(map)
      out
    }
  }

}

