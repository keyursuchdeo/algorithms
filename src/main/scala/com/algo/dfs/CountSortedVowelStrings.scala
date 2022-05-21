package com.algo.dfs

object CountSortedVowelStrings extends App {
  object Solution {
    def countVowelStrings(n: Int): Int = {

      var map: Map[(Int, Int, Int), Int] = Map[(Int, Int, Int), Int]()

      def count(currN: Int, fromIndex: Int, toIndex: Int): Int = {
        if(currN == 1) {
          (toIndex - fromIndex + 1)
        } else {
          map.get((currN, fromIndex, toIndex)) match {
            case Some(value) => value
            case None =>
              val value =
                (fromIndex to toIndex).map(index => {
                  count(currN - 1, index, 4)
                }).sum
              map = map + ((currN, fromIndex, toIndex) -> value)
              value
          }
        }
      }
      count(n, 0, 4)
    }
  }
}
