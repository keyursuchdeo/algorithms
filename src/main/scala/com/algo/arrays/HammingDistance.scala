package com.algo.arrays

object HammingDistance extends App {

  val res = Solution.hammingDistance(1, 4)
  println(res)

  object Solution {
    def hammingDistance(x: Int, y: Int): Int = {
      if (x == y) {
        0
      } else {
        val xBin = x.toBinaryString
        val yBin = y.toBinaryString
        val xBinLen = xBin.length
        val yBinLen = yBin.length
        val (eqXBin, eqYBin) =
          if (xBinLen > yBinLen) {
            (xBin, prependZeroes(xBinLen - yBinLen, yBin))
          } else if (xBinLen < yBinLen) {
            (prependZeroes(yBinLen - xBinLen, xBin), yBin)
          } else {
            (xBin, yBin)
          }

        findDistance(eqXBin.toList, eqYBin.toList, 0)
      }
    }

    @scala.annotation.tailrec
    def findDistance(xBin: Seq[Char], yBin: Seq[Char], distance: Int): Int = {
      (xBin, yBin) match {
        case (x :: xs, y :: ys) if x == y =>
          findDistance(xs, ys, distance)
        case (x :: xs, y :: ys) if x != y =>
          findDistance(xs, ys, distance + 1)
        case (_, _) => distance
      }
    }

    def prependZeroes(zeroCount: Int, str: String): String = {
      val sb = new StringBuilder()
      (0 until zeroCount).foreach(_ => sb.append('0'))
      sb.append(str).toString()
    }
  }

}
