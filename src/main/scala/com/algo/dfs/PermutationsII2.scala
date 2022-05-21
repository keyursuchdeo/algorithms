package com.algo.dfs

object PermutationsII2 extends App {

  val a = Array[Int](1, 2, 3)
  val res = Solution.permuteUnique(a)
  println(res)

  object Solution {
    def permuteUnique(nums: Array[Int]): List[List[Int]] = {
      def find() = {
        if(nums.isEmpty) {
          Nil
        } else {
          nums.indices.flatMap(index => {
            val (before, after) = nums.splitAt(index)
            val output = permuteUnique(before ++ after.tail)
            output.map(after.head +: _)
          }).toList
        }
      }

      if (nums.length <= 1) List(nums.toList) else find()
    }
  }
}
