package com.algo.aws2.missingint

import com.algo.aws2.maxsum.Solution

class Solution {
  def firstMissingPositive(A: Array[Int]): Int  = {
    val len = A.length
    val buff = Array.ofDim[Int](len)
    println("----------")
    println(buff.mkString(","))
    println("----------")
    A.foreach(ai => if (ai > 0 && ai <= len) buff(ai - 1) = buff(ai - 1) + 1)

    println("----------")
    println(buff.mkString(","))
    println("----------")

    for {
      i <- buff.indices
      ai = buff(i)
      if ai == 0
    } return i + 1
    len + 1
  }
}

object MissingMain extends App {
  val sol = new Solution()
//  val a = Array(1, 2, 0)
  val a = Array(3, 4, -1, 1)
//  val a = Array(-8, -7, -6)
//  val a = Array(2, 1, 3)
//  val a = Array(3, 1, 2)
  val missing = sol.firstMissingPositive(a)
  println(missing)

}

