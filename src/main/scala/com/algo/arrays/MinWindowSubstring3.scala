package com.algo.arrays

object MinWindowSubstring3 extends App {

  val s1 = "cabwefgewcwaefgcf"
  val t1 = "cae"

  val res = Solution.minWindow(s1, t1)
  println(res)

  object Solution {
    def minWindow(s: String, t: String): String = {

      @scala.annotation.tailrec
      def find(low: Int, high: Int, sMap: Map[Char, Int], tMap: Map[Char, Int]): (Int, Int) = {
        sMap.get(s(low)) match {
          case Some(lowValue) if lowValue == tMap(s(low)) =>
            sMap.get(s(high)) match {
              case Some(highValue) if highValue == tMap(s(high)) => (low, high)
              case Some(highValue) => find(low, high - 1, sMap + (s(high) -> (highValue - 1)), tMap)
              case _ => find(low, high - 1, sMap, tMap)
            }
          case Some(lowValue) => find(low + 1, high, sMap + (s(low) -> (lowValue - 1)), tMap)
          case _ => find(low + 1, high, sMap, tMap)
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

      def prepCharMapGivenT(str: String, tMap: Map[Char, Int]): Map[Char, Int] = {
        @scala.annotation.tailrec
        def prep(index: Int, map: Map[Char, Int]): Map[Char, Int] = {
          if (index == str.length) {
            map
          } else {
            tMap.get(str(index)) match {
              case Some(_) =>
                map.get(str(index)) match {
                  case Some(value) => prep(index + 1, map + (str(index) -> (value + 1)))
                  case _ => prep(index + 1, map + (str(index) -> 1))
                }
              case None => prep(index + 1, map)
            }
          }
        }
        prep(0, Map())
      }

      if(t.isEmpty || s.isEmpty) {
        ""
      } else {
        val tMap: Map[Char, Int] = prepCharMap(t)
        val sMap = prepCharMapGivenT(s, tMap)
        if(containsAllChars(sMap, tMap)) {
          val (low, high) = find(0, s.length - 1, sMap, tMap)
          s.substring(low, high + 1)
        } else {
          ""
        }
      }
    }
  }

}
