package com.algo.arrays

object NumsWithSameConsecutiveDiff extends App {

  val res = Solution.numsSameConsecDiff(7, 4)
  println(res.mkString(","))

  object Solution {
    def numsSameConsecDiff(N: Int, K: Int): Array[Int] = {

      def digits(currN: Int, currNum: Seq[Int]): Seq[Int] = {
        if (currN == 0) {
          Seq(currNum.reverse.mkString("").toInt)
        } else {
          (0 to 9).collect {
            case digit if Math.abs(digit - currNum.head) == K =>
              digits(currN - 1, digit +: currNum)
          }.flatten
        }
      }


      if (N == 1) {
        Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      } else {
        (1 to 9).flatMap(digit => digits(N - 1, Seq(digit))).toArray
      }
    }
  }

}
