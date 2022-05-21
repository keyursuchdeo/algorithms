package com.algo.arrays

import scala.collection.mutable

object DecodeWays2 extends App {

  val st = "234"
  val res = Solution.numDecodings(st)
  println(res)

  object Solution {
    var map: mutable.Map[String, Int] = mutable.Map[String, Int]()

    def numDecodings(s: String): Int = {
      if (s.isEmpty) {
        1
      } else if (s.length == 1) {
        if (s.toInt == 0) {
          0
        } else {
          1
        }
      } else if (s.startsWith("0")) {
        0
      } else {
        map.get(s) match {
          case Some(value) => value
          case _ =>
            val first2Digits = s"${s.head}${s.tail.head}".toInt
            val value =
              if (first2Digits > 10 && first2Digits < 27) {
                numDecodings(s.tail) + numDecodings(s.tail.tail)
              } else {
                numDecodings(s.tail)
              }
            map = map + (s -> value)
            value
        }
      }
    }
  }

}
