package com.algo.dfs

object PermutationsII extends App {

  val a = Array[Int](1, 2, 1)
  val res = Solution.permuteUnique(a)
  println(res)

  object Solution {
    def permuteUnique(nums: Array[Int]): List[List[Int]] = {
      def findUniquePermutations(currNums: Array[Int]): List[List[Int]]  = {
        var consideredNums = Set[Int]()
        if (currNums.length <= 1) {
          List(currNums.toList)
        } else {
          currNums.indices.collect{
            case index if !consideredNums.contains(currNums(index)) =>
              val (before, after) = currNums.splitAt(index)
              val output = findUniquePermutations(before ++ after.tail)
              consideredNums = consideredNums + after.head
              output.map(after.head +: _)
          }.flatten.toList
        }
      }

      findUniquePermutations(nums)
    }
  }
}
