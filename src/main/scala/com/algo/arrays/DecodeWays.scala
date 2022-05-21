package com.algo.arrays

object DecodeWays extends App {

  val st = "234"
  val res = Solution.numDecodings(st)
  println(res)

  object Solution {
    def numDecodings(s: String): Int = {
      if(s.length == 0) {
        0
      } else {
        val sNum = s.toInt
        if (s.length == 1) {
          if (sNum == 0) 0 else 1
        } else {
          val a = numDecodings(s.head.toString) * numDecodings(s.tail)
          val twoDigitCode = s"${s.head}${s.tail.head}"
          val b =
            if (valid2DigitCode(twoDigitCode)) {
              if(s.tail.tail.nonEmpty) {
                1 * numDecodings(s.tail.tail)
              } else {
                1
              }
            } else {
              0
            }
          a + b
        }
      }
    }

    def valid2DigitCode(str: String): Boolean = {
      if (str.head == '0') {
        false
      } else {
        val sNum = str.toInt
        if (sNum > 20 & sNum % 10 == 0) {
          false
        } else {
          true
        }
      }
    }
  }

}
