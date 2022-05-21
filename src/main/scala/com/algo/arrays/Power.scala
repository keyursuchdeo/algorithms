package com.algo.arrays

object Power extends App {

  val res = Solution.myPow(2.0, 10)
  println(res)

  object Solution {
    def myPow(x: Double, n: Int): Double = {
      def find(currN: Int): Double = {
        if(currN == 0) {
          1
        } else if (currN == 1 || x == 1) {
          x
        } else {
          val a = find(currN / 2)
          if(currN % 2 == 0)  {
            a * a
          } else {
            a * a * x
          }

        }
      }

      val out = find(Math.abs(n))
      if(n < 0) 1 / out else out
    }
  }

}
