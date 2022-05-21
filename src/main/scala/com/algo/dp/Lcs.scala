package com.algo.dp

object Lcs extends App {
  val s1 = "bd"
  val s2 = "abcd"

  Solution.lcs(s1, s2)

  object Solution {
    def lcs(s1: String, s2: String): Int = {
      val matrix: Array[Array[Int]] = prepDefaultMatrix(s1.length, s2.length)

      def find(s1Index: Int, s2Index: Int): Int = {
        if(s1Index == s1.length || s2Index == s2.length) {
          0
        } else if (matrix(s1Index)(s2Index) != -1) {
          matrix(s1Index)(s2Index)
        } else {
          if(s1(s1Index) == s2(s2Index)) {
            val b = 1 + find(s1Index + 1, s2Index + 1)
            matrix(s1Index)(s2Index) = b
            b
          } else {
            val a = Math.max(find(s1Index + 1, s2Index), find(s1Index, s2Index + 1))
            println(s"${s1(s1Index)} ${s2(s2Index)} $a")
            matrix(s1Index)(s2Index) = a
            a
          }
        }
      }
      val maxLen = find(0, 0)
      println(maxLen)
      matrix.foreach(i => println(i.mkString(",")))
      maxLen
    }

    private def prepDefaultMatrix(s1Length: Int, s2Length: Int): Array[Array[Int]] = {
      val matrix: Array[Array[Int]] = Array.ofDim[Int](s1.length, s2.length)
      for (
        i <- 0 until s1Length;
        j <- 0 until s2Length
      ) {
        matrix(i)(j) = -1
      }
      matrix
    }

  }
}
