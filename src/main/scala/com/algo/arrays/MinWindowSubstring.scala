package com.algo.arrays

object MinWindowSubstring extends App {

  val s1 = "ADOBECODEBANC"
  val t1 = "ABC"

  val res = Solution.minWindow(s1, t1)
  println(res)

  object Solution {
    def minWindow(s: String, t: String): String = {

      @scala.annotation.tailrec
      def find(low: Int, high: Int, substringIndex: Seq[Seq[Int]], currSubstringIndex: Vector[Int], tMap: Set[Char]): String = {
        if (high < low) {
          val minSub = substringIndex.minBy(_.length)
          s.substring(minSub.head, minSub.reverse.head + 1)
        } else {
          if (tMap.isEmpty) {
            if (high == s.length - 1) {
              find(
                currSubstringIndex.tail.head,
                low,
                currSubstringIndex +: substringIndex,
                currSubstringIndex.tail,
                tMap + s(currSubstringIndex.head))
            } else {
              find(
                currSubstringIndex.tail.head,
                high,
                currSubstringIndex +: substringIndex,
                currSubstringIndex.tail,
                tMap + s(currSubstringIndex.head))
            }
          } else {
            if (high == s.length - 1) {
              if (tMap.contains(s(low))) {
                find(low + 1, high, substringIndex, currSubstringIndex :+ low, tMap - s(low))
              } else {
                find(low + 1, high, substringIndex, currSubstringIndex, tMap)
              }
            } else {
              if(tMap.contains(s(high))) {
                find(low, high + 1, substringIndex, currSubstringIndex :+ high, tMap - s(high))
              } else {
                find(low, high + 1, substringIndex, currSubstringIndex, tMap)
              }
          }
        }
      }
    }

      find(0, s.length - 1, Nil, Vector(), t.toSet)
  }

}

}
