package com.algo.dp

object FourSumII extends App {

  object Solution {
    def fourSumCount(A: Array[Int], B: Array[Int], C: Array[Int], D: Array[Int]): Int = {

      var map = Map[(Int, Int), Int]()

      def count(arrays: Seq[Array[Int]], len: Int, currSum: Int): Int = {
        if (arrays.isEmpty && currSum == 0) {
          1
        } else if (arrays.isEmpty) {
          0
        } else {
          map.get((len, currSum)) match {
            case Some(value) => value
            case None =>
              val array = arrays.head
              val value =
                array.map(element => {
                  count(arrays.tail, len - 1, currSum + element)
                }).sum
              map = map + ((len, currSum) -> value)
              value
          }

        }
      }

      count(Seq(A, B, C, D), 4, currSum = 0)
    }
  }

}
