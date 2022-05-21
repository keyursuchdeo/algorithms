package com.algo.arrays

object MinWindowSubstring2 extends App {

  val s1 = "ADOBECODEBANC"
  val t1 = "ABC"

  val res = Solution.minWindow(s1, t1)
  println(res)

  object Solution {
    def minWindow(s: String, t: String): String = {
      @scala.annotation.tailrec
      def find(low: Int, high: Int, currT: Map[Char, Int], currRevSubstring: Seq[(Char, Int)], minSubstring: Seq[Char]): String = {
        if(currT.isEmpty) {
          val currSubstringWithIndex = currRevSubstring.reverse
          val newLow = currSubstringWithIndex.tail.head._2
          val newHigh = currRevSubstring.head._2
          val newCurrTChar = currSubstringWithIndex.head._1
          val currSubstring = currSubstringWithIndex.map(_._1)
          if(minSubstring.isEmpty || currSubstring.size < minSubstring.size) {
            find(newLow, newHigh + 1, currT + (newCurrTChar -> 1), currSubstringWithIndex.tail.reverse, currSubstring)
          } else {
            find(newLow, newHigh + 1, currT + (newCurrTChar -> 1), currSubstringWithIndex.tail.reverse, minSubstring)
          }
        } else if (high == s.length - 1) {
          currT.get(s(low)) match {
            case None =>
              if(currRevSubstring.isEmpty) {
                find(low + 1, high, currT, currRevSubstring, minSubstring)
              } else {
                find(low + 1, high, currT, (s(low), low) +: currRevSubstring, minSubstring)
              }
            case Some(num) if num == 1 =>
              find(low + 1, high, currT - s(low), (s(low), low) +: currRevSubstring, minSubstring)
            case Some(num) =>
              find(low + 1, high, currT + (s(low) -> (num - 1)), (s(low), low) +: currRevSubstring, minSubstring)
          }
        } else if (low < high) {
          currT.get(s(high)) match {
            case None =>
              if(currRevSubstring.isEmpty) {
                find(low, high + 1, currT, currRevSubstring, minSubstring)
              } else {
                find(low, high + 1, currT, (s(high), high) +: currRevSubstring, minSubstring)
              }
            case Some(num) if num == 1 =>
              find(low, high + 1, currT - s(high), (s(high), high) +: currRevSubstring, minSubstring)
            case Some(num) =>
              find(low, high + 1, currT + (s(high) -> (num - 1)), (s(high), high) +: currRevSubstring, minSubstring)
          }
        } else {
          minSubstring.toString()
        }
      }

      def prepCharMap(str: String): Map[Char, Int] = {
        @scala.annotation.tailrec
        def prep(index: Int, map: Map[Char, Int]): Map[Char, Int] = {
          if(index == str.length) {
            map
          } else {
            map.get(str(index)) match {
              case Some(value) => prep(index + 1, map + (str(index) -> (value + 1)))
              case _ => prep(index + 1, map + (str(index) -> 1))

            }

          }
        }
        prep(0, Map())
      }

      find(0, s.length - 1, prepCharMap(t), Nil, Nil)
    }

  }

}
