package com.algo.arrays

object BagOfTokens extends App {

  object Solution {
    def bagOfTokensScore(tokens: Array[Int], P: Int): Int = {
      @scala.annotation.tailrec
      def maximizeScore(low: Int, high: Int, score: Int, currPower: Int, sortedTokens: Array[Int]): Int = {
        if(low == tokens.length) {
          score
        } else {
          if (score == 0) {
            if (currPower >= sortedTokens(low)) {
              maximizeScore(low + 1, high, score + 1, currPower - sortedTokens(low), sortedTokens)
            } else {
              score
            }
          } else {
            if (currPower >= sortedTokens(low)) {
              maximizeScore(low + 1, high, score + 1, currPower - sortedTokens(low), sortedTokens)
            } else {
              if(high - 1 < low) {
                score
              } else {
                maximizeScore(low, high - 1, score - 1, currPower + sortedTokens(high), sortedTokens)
              }
            }
          }
        }
      }

      if (tokens.isEmpty) 0 else maximizeScore(0, tokens.length - 1, 0, P, tokens.sorted)
    }
  }

}
