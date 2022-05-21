package com.algo.dp

object PalindromePartitioningII3 extends App {

  val res = Solution.minCut("cabababc")
  println(res)

  object Solution {
    def minCut(s: String): Int = {

      val cuts: Array[Array[Int]] = Array.fill[Int](s.length, s.length)(-1)
      val str: Array[(Char, Int)] = s.toCharArray.zipWithIndex

      def initializeCuts(): Unit = {
        for {
          row <- 0 until s.length;
          col <- 0 until s.length;
          if row == col
        } {
          cuts(row)(col) = 0
        }
      }

      def pCuts(currString: Array[(Char, Int)]): Unit = {
        val (_, hIndex) = currString.head
        val (_, lIndex) = currString(currString.length - 1)

        if (lIndex == hIndex) {
          cuts(lIndex)(lIndex) = 0
        } else {
          (lIndex to hIndex by -1).foreach(index => {
            if (index == lIndex) {
              cuts(index)(index) = 0
            } else if (lIndex - index == 1) {
              cuts(index)(lIndex) = if (currString(lIndex)._1 == currString(index)._1) 0 else 1
            } else {
              cuts(index)(lIndex) =
                if (currString(lIndex)._1 == currString(index)._1 && cuts(index + 1)(lIndex - 1) == 0) {
                  0
                } else {
                  (index until lIndex).map(innerIndex => {
                    1 + cuts(index)(innerIndex) + cuts(innerIndex + 1)(lIndex)
                  }).min
                }
            }
          })
        }
      }

      @scala.annotation.tailrec
      def find(index: Int): Unit = {
        if (index == str.length) {
          ()
        } else {
          pCuts(str.take(index + 1))
          find(index + 1)
        }
      }

      initializeCuts()
      find(0)
      println(cuts.map(_.mkString(",")).mkString("|"))
      cuts(0)(s.length - 1)

    }
  }

}
