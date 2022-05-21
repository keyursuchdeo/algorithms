package com.algo.arrays

object MinWindowSubstring4 extends App {

//  val s1 = "cabwefgewcwaefgcf"
//  val t1 = "cae"

  val s1 = "a"
  val t1 = "aa"

  val res = Solution.minWindow(s1, t1)
  println(res)

  object Solution {
    def minWindow(s: String, t: String): String = {

      @scala.annotation.tailrec
      def find(low: Int, high: Int, sMap: Map[Char, Int], tMap: Map[Char, Int], bestWindowL: Int, bestWindowH: Int): (Int, Int) = {
        if (low == s.length || high == s.length) {
          (bestWindowL, bestWindowH)
        } else {
          if (containsAllChars(sMap, tMap)) {
            val (newBestL, newBestH) =
              if (bestWindowH - bestWindowL < high - low) {
                (bestWindowL, bestWindowH)
              } else {
                (low, high)
              }
            sMap.get(s(low)) match {
              case Some(value) if value == 1 => find(low + 1, high, sMap - s(low), tMap, newBestL, newBestH)
              case Some(value) => find(low + 1, high, sMap + (s(low) -> (value - 1)), tMap, newBestL, newBestH)
              case _ => find(low + 1, high, sMap, tMap, newBestL, newBestH)
            }
          } else {
            if (high + 1 == s.length) {
              (bestWindowL, bestWindowH)
            } else {
              find(low, high + 1, sMap + (s(high + 1) -> (sMap.getOrElse(s(high + 1), 0) + 1)), tMap, bestWindowL, bestWindowH)
            }
          }

        }
      }

      def containsAllChars(sMap: Map[Char, Int], tMap: Map[Char, Int]) = {
        !tMap.keySet.exists(key => {
          tMap(key) > sMap.getOrElse(key, 0)
        })
      }

      def prepCharMap(str: String): Map[Char, Int] = {
        @scala.annotation.tailrec
        def prep(index: Int, map: Map[Char, Int]): Map[Char, Int] = {
          if (index == str.length) {
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

      if (t.isEmpty || s.isEmpty || s.length < t.length) {
        ""
      } else {
        val tMap: Map[Char, Int] = prepCharMap(t)
        val (low, high) = find(0, 0, Map(s.head -> 1), tMap, 0, s.length)
        if (low >= 0) s.substring(low, high + 1) else ""
      }
    }
  }

}
