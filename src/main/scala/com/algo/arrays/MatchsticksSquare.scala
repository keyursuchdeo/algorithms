package com.algo.arrays

object MatchsticksSquare extends App {
  object Solution {
    def makesquare(matchsticks: Array[Int]): Boolean = {
      def make(index: Int, len1: Int, len2: Int, len3: Int, len4: Int, reqLen: Int): Boolean = {
        if(index == matchsticks.length) {
          len1 == len2 && len2 == len3 && len3 == len4 && len4 == reqLen
        } else {
          if(len1 > reqLen || len2 > reqLen || len3 > reqLen || len4 > reqLen) {
            false
          } else {
            make(index + 1, len1 + matchsticks(index), len2, len3, len4, reqLen) ||
              make(index + 1, len1, len2 + matchsticks(index), len3, len4, reqLen) ||
              make(index + 1, len1, len2, len3 + matchsticks(index), len4, reqLen) ||
              make(index + 1, len1, len2, len3, len4 + matchsticks(index), reqLen)
          }
        }
      }

      val reqLen = matchsticks.sum / 4.0
      if(reqLen == reqLen.toInt) {
        make(0, 0, 0, 0, 0, reqLen.toInt)
      } else {
        false
      }
    }
  }
}
