package com.algo.arrays

object CombinationSumII extends App {

//  val a = Array(10,1,2,7,6,1,5)
//  val t = 8

  val a = Array(2, 5, 2, 1, 2)
  val t = 5
  val res = Solution.combinationSum2(a, t)
  println(res)


  object Solution {
    def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
      def findCombinations(currCandidates: List[Int], remainingTarget: Int, currCombination: List[Int]): List[List[Int]] = {
        if(remainingTarget == 0) {
          List(currCombination)
        } else if (currCandidates.isEmpty) {
          List(Nil)
        } else {
          if(currCandidates.head <= remainingTarget) {
            findCombinations(currCandidates.tail, remainingTarget, currCombination) ++
            findCombinations(currCandidates.tail, remainingTarget - currCandidates.head, currCandidates.head +: currCombination)
          } else {
            findCombinations(currCandidates.tail, remainingTarget, currCombination)
          }
        }
      }

      findCombinations(candidates.toList, target, Nil).filter(_.nonEmpty).map(_.sorted).distinct


    }
  }
}
