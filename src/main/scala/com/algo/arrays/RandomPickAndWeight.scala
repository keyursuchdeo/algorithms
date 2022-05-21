package com.algo.arrays

import scala.util.Random

object RandomPickAndWeight extends App {
  class Solution(_w: Array[Int]) {

    @scala.annotation.tailrec
    private def prepCumSum(index: Int, a: Array[Int]): Array[Int] = {
      if(index == _w.length) {
        a
      } else if (index == 0) {
        a(index) = _w(index)
        prepCumSum(index + 1, a)
      } else {
        a(index) = _w(index) + a(index - 1)
        prepCumSum(index + 1, a)
      }
    }

    private lazy val cumulativeSums: Array[Int] = prepCumSum(0, new Array[Int](_w.length))


    def pickIndex(): Int = {
      val target = Random.nextInt(cumulativeSums(_w.length - 1))
      @scala.annotation.tailrec
      def pick(index: Int): Int = {
        if(target < cumulativeSums(index)) {
          index
        } else {
          pick(index + 1)
        }
      }

      pick(0)
    }

  }
}
