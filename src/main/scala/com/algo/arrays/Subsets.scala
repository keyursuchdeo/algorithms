package com.algo.arrays

object Subsets extends App {
  object Solution {
    def subsets(nums: Array[Int]): List[List[Int]] = {
      def find(currNums: Array[Int]): List[List[Int]] = {
        if(currNums.length == 1) {
          List(currNums.toList, Nil)
        } else {
          val tailSubsets = find(nums.tail)
          tailSubsets ++ tailSubsets.map(s => nums.head +: s)
        }
      }

      if(nums.isEmpty) Nil else find(nums)
    }
  }
}
