package com.algo.dp

object StoneGameIV extends App {
  object Solution {
    def winnerSquareGame(n: Int): Boolean = {
      val winners = new Array[Int](n + 1)

      def isSquareNum(num: Int): Boolean = {
        val sqRoot: Double = Math.sqrt(num)
        sqRoot.toInt == sqRoot
      }

      @scala.annotation.tailrec
      def fillWinners(currN: Int, prevSquares: Seq[Int]): Unit = {
        if(currN == n + 1) {
          ()
        } else {
          if(isSquareNum(currN)) {
            winners(currN) = 1
            fillWinners(currN + 1, currN +: prevSquares)
          } else {
            prevSquares.find(sq => {
              winners(currN - sq) == 0
            }) match {
              case Some(_) =>
                winners(currN) = 1
              case _ =>
                winners(currN) = 0
            }
            fillWinners(currN + 1, prevSquares)
          }

        }
      }

      fillWinners(1, Nil)
      winners(n) == 1
    }
  }
}
