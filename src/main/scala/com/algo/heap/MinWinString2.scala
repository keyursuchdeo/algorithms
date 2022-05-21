package com.algo.heap


object MinWinString2 extends App {

  val s = "ADOBECODEBANC"
  val t = "ABC"

  val res = Solution.minWindow(s, t)
  println(res)

  object Solution {
    def minWindow(s: String, t: String): String = {

      def countChars(str: String) = {
        @scala.annotation.tailrec
        def count(index: Int, map: Map[Char, Int]): Map[Char, Int] = {
          if(index == str.length) {
            map
          } else {
            count(index + 1, map + (str(index) -> (map.getOrElse(str(index), 0) + 1)))
          }
        }
        count(0, Map.empty)
      }


      val tCharCount: Map[Char, Int] = countChars(t)

      @scala.annotation.tailrec
      def min(low: Int, high: Int, minWin: Option[String], sCharCount: Map[Char, Int]): Option[String] = {
        val str = s.substring(low, high)
        if(containsTChars(sCharCount)) {
          val newMinWin = minWin match {
            case Some(string) =>
              if (str.length < string.length) Some(str) else minWin
            case None => Some(str)
          }
          val char = s(high - 1)
          val updatedCount = sCharCount(char) - 1
          min(low, high - 1, newMinWin, sCharCount + (char -> updatedCount))
        } else if (high < s.length) {
          val lChar = s(low + 1)
          val updatedLCount = sCharCount(lChar) + 1

          val hChar = s(high)
          val updatedHCount = sCharCount(hChar) + 1

          min(low + 1, high + 1, minWin, sCharCount + (lChar -> updatedLCount, hChar -> updatedHCount))
        } else {
          minWin
        }
      }

//      def containsTChars(str: String) = {
//        @scala.annotation.tailrec
//        def contains(index: Int, containsChar: Boolean): Boolean = {
//          if (index == t.length || !containsChar) {
//            containsChar
//          } else {
//            contains(index + 1, str.contains(t(index)))
//          }
//        }
//        contains(index = 0, containsChar = true)
//      }

      def containsTChars(sCharCount: Map[Char, Int]) = {

        @scala.annotation.tailrec
        def contains(index: Int, containsChar: Boolean): Boolean = {
          if (index == t.length || !containsChar) {
            containsChar
          } else {
            contains(index + 1, sCharCount.getOrElse(t(index), 0) >= tCharCount.getOrElse(t(index), 0))
          }
        }
        contains(index = 0, containsChar = true)
      }

      min(0, s.length, None, countChars(s)).getOrElse("")
    }
  }
}
