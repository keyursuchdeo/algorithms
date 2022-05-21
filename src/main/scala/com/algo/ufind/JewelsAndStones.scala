package com.algo.ufind

object JewelsAndStones extends App {

  val J = ""
  val S = ""

  val res = Solution.numJewelsInStones(J, S)
  println(res)

  object Solution {
    def numJewelsInStones(J: String, S: String): Int = {
      val jewels: Set[Char] = J.toSet

      @scala.annotation.tailrec
      def countJewels(index: Int, count: Int): Int = {
        if(index == S.length) {
          count
        } else {
          if(jewels.contains(S(index))) {
            countJewels(index + 1, count + 1)
          } else {
            countJewels(index + 1, count)
          }
        }
      }

      countJewels(0, 0)
    }
  }
}
