package com.algo.arrays

object DistributeCandies extends App {
  object Solution {
    def distributeCandies(candyType: Array[Int]): Int = {
      val typeCount = candyType.toSet.size
      if (typeCount >= candyType.length / 2) candyType.length / 2 else typeCount
    }
  }
}
