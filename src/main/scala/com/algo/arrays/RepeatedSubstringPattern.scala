package com.algo.arrays


object RepeatedSubstringPattern extends App {

  val st = "abacababacab"
  val res = Solution.repeatedSubstringPattern(st)
  println(res)

  object Solution {
    def repeatedSubstringPattern(s: String): Boolean = {
      (s + s).substring(1, 2 * (s.length) - 1).contains(s)
    }
  }
}
