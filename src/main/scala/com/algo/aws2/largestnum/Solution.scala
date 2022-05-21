package com.algo.aws2.largestnum

class Solution {
  def largestNumber(a: Array[Int]): String  = {
    val out = a.filter(_ > 0).sortWith(compare).mkString("")
    if (out.isEmpty) "0" else out
  }

  private def compare(num1: Int, num2: Int): Boolean = {
    concatNums(num1, num2) > concatNums(num2, num1)
  }

  private def concatNums(num1: Int, num2: Int): Long = {
    num1.toString.concat(num2.toString).toLong
  }

}

object LargestNumMain extends App {
  val sol = new Solution()
  val a = Array(3, 30, 34, 5, 9)
//  val a = Array(3, 30)
//  val a = Array(12, 121, 1221)
//  val a = Array(0, 0, 0, 0, 0)
  val num = sol.largestNumber(a)
  println(num)
}

