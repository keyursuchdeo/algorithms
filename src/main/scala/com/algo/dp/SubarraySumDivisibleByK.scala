package com.algo.dp

object SubarraySumDivisibleByK extends App {

  val a = Array(8,9,7,8,9)
  val res = Solution.subarraysDivByK(a, 8)
  println(res)

  object Solution {
    def subarraysDivByK(A: Array[Int], K: Int): Int = {

      val counts = new Array[Int](K)

      @scala.annotation.tailrec
      def fillCounts(index: Int, currSum: Int): Unit = {
        if(index == A.length) {
          ()
        } else {
          val sum = currSum + A(index)
          counts(Math.abs(sum % K)) = counts(Math.abs(sum % K)) + 1
          fillCounts(index + 1, sum)
        }
      }

      fillCounts(0, 0)
      counts.head + counts.map(count => {
        (count * (count - 1)) / 2
      }).sum

    }
  }

}
