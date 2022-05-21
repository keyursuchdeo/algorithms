package com.algo.dfs

object CoinChange extends App {

  val a = 11
  val c = Array(1, 2, 5)
  val res = Solution.coinChange(c, a)
  println(res)

  object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      val array = Array.fill[Int](coins.length, amount + 1)(-1)

      def find(currAmount: Int, remainingCoins: Seq[(Int, Int)], count: Int): Int = {
        if (currAmount == amount) {
          count
        } else if (currAmount > amount) {
          Int.MaxValue
        } else {
          remainingCoins.headOption match {
            case Some((headCoin, headCoinIndex)) =>
              if(array(headCoinIndex)(currAmount) != -1) {
                array(headCoinIndex)(currAmount)
              } else {
                array(headCoinIndex)(currAmount)  =
                  Math.min(find(currAmount + headCoin, remainingCoins, count + 1), find(currAmount, remainingCoins.tail, count))
                array(headCoinIndex)(currAmount)
              }
            case None =>
              Int.MaxValue
          }
        }
      }

      val b = find(0, coins.toSeq.filter(_ <= amount).zipWithIndex, 0)
      println(array.map(_.mkString(",")).mkString("||"))
      b
    }
  }

}
