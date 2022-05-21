package com.algo.dp

object BinaryTreesWithFactors2 extends App {

  object Solution {
    def numFactoredBinaryTrees(arr: Array[Int]): Int = {
      val arrSet = arr.toSet
      var ansByNum: Map[Int, Long] = Map[Int, Long]()
      val N = Math.pow(10, 9).toInt + 7

      def dp(num: Int): Long = {
        ansByNum.get(num) match {
          case Some(value) => value
          case None =>
            var ans = 1L
            for(cand <- arrSet) {
              if(num % cand == 0 && arrSet.contains(num / cand)) {
                ans = ans + (dp(cand) * dp(num / cand))
              }
            }
            ansByNum = ansByNum + (num -> ans)
            ans
        }
      }


      (arrSet.map(dp).sum % N).toInt

    }
  }

}