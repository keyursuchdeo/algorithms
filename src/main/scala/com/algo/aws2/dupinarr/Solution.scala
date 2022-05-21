package com.algo.aws2.dupinarr

import scala.collection.BitSet
import scala.collection.immutable.HashSet

class Solution {
  def repeatedNumber(a: Array[Int]): Int  = {
    @scala.annotation.tailrec
    def check(index: Int, set: Set[Int]): Int = {
      if (index == a.length) {
        -1
      } else {
        if (set.contains(a(index))) {
          a(index)
        } else {
          check(index + 1, set + a(index))
        }
      }
    }

    check(0, Set.empty)
  }
}

object RepMain extends App {
  val sol = new Solution()
  val a = Array(3, 4, 1, 5, 2)
  val o = sol.repeatedNumber(a)
  println(o)
}

