package com.algo.arrays

object PermutationSequence extends App {
  val num = 4
  val per = 10
  val res = Solution.getPermutation(num, per)
  println(res)

  object Solution {
    def getPermutation(n: Int, k: Int): String = {

      val factorials = new Array[Int](n + 1)

      @scala.annotation.tailrec
      def calculateFactorial(currN: Int): Unit = {
        if (currN <= 1) {
          factorials(currN) = currN
          calculateFactorial(currN + 1)
        } else if (currN > n) {
          ()
        } else {
          factorials(currN) = currN * factorials(currN - 1)
          calculateFactorial(currN + 1)
        }
      }

      @scala.annotation.tailrec
      def calculatePermutation(currN: Int, currK: Int, currSeq: Array[Int], permutation: String): String = {
        if(currK == 1) {
          permutation + currSeq.mkString("")
        } else if(currK == factorials(currN)) {
          permutation + currSeq.reverse.mkString("")
        } else {
          val nIndex = (Math.ceil(currK.toDouble / factorials(currN - 1)) - 1).toInt
          val newK = if (currK % factorials(currN - 1) == 0) factorials(currN - 1) else currK % factorials(currN - 1)
          val before = currSeq.take(nIndex)
          val after = currSeq.drop(nIndex + 1)
          calculatePermutation(currN - 1, newK, before ++ after, permutation + currSeq(nIndex))

        }
      }

      calculateFactorial(0)
      calculatePermutation(n, k, (1 to n).toArray, "")
    }
  }
}
