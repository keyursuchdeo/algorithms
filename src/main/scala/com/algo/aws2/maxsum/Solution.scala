package com.algo.aws2.maxsum

class Solution {
  def maxSubArray(a: Array[Int]): Int  = {

   @scala.annotation.tailrec
   def prepMaxSumsByIndex(index: Int, maxSums: Seq[Int]): Seq[Int] = {
      if(index == a.length) {
        maxSums
      } else {
        val currIndexMax = calcCurrIndexMax(maxSums.head, index)
        prepMaxSumsByIndex(index + 1, currIndexMax +: maxSums)
      }
   }

   def calcCurrIndexMax(prevIndexMax: Int, index: Int): Int = {
     val currSum = prevIndexMax + a(index)
     if (currSum > a(index)) currSum else a(index)
   }

    val maxSums = prepMaxSumsByIndex(1, Seq(a.head))
    maxSums.max
  }
}

object Main extends App {
  val sol = new Solution()
//  val a = Array(1, 2, 3, 4, -10)
//  val a = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)
  val a = Array(-2, 1, -3, 4)
  val maxSum = sol.maxSubArray(a)
  println(a.mkString(","))
  println(maxSum)

}

