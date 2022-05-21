package com.algo.dp

import scala.collection.mutable

object PalindromePartitioningII extends App {

  val res = Solution.minCut("abbab")
  println(res)

  object Solution {
    def minCut(s: String): Int = {

      def isPalindrome(str: String): Boolean = {
        str == str.reverse
      }

      var map = mutable.Map[String, Int]()
      var map1 = mutable.Map[Int, Int]()

      def calculateMin(prefixS: String, suffixS: String, index: Int): Int = {
        if (suffixS.isEmpty) {
          if (prefixS.nonEmpty) {
            Int.MaxValue
          } else {
            -1
          }
        } else {
          val currS = prefixS + suffixS
          map.get(currS) match {
            case Some(minValue) => minValue
            case _ =>
              val combinedS = prefixS + suffixS.head
              val minValue =
                if (isPalindrome(combinedS)) {
                  Math.min(
                    calculateMin(combinedS, suffixS.tail, index + 1),
                    1 + calculateMin("", suffixS.tail, index + 1)
                  )
                } else {
                  calculateMin(combinedS, suffixS.tail, index + 1)
                }
              map = map + (currS -> minValue)
              println(s"$index -> $minValue")
              map1 = map1 + (index -> minValue)
              minValue
          }
        }
      }

      val a = if (s.isEmpty || isPalindrome(s)) 0 else calculateMin("", s, 0)
      println(map)
      println(map1)
      a

    }
  }

}
