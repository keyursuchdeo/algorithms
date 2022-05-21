package com.algo.arrays

object CombinationSumIII extends App {

  val res = Solution.combinationSum3(2, 9)
  println(res)

  object Solution {
    def combinationSum3(k: Int, n: Int): List[List[Int]] = {
      def findCombinations(currCandidates: List[Int], remainingN: Int, remainingK: Int, currCombination: List[Int]): List[List[Int]] = {
        if(remainingN == 0 && remainingK == 0) {
          List(currCombination)
        } else if (currCandidates.isEmpty || remainingN == 0 || remainingK == 0) {
          List(Nil)
        } else {
          if(currCandidates.head <= remainingN) {
            findCombinations(currCandidates.tail, remainingN, remainingK, currCombination) ++
              findCombinations(currCandidates.tail, remainingN - currCandidates.head, remainingK - 1, currCandidates.head +: currCombination)
          } else {
            findCombinations(currCandidates.tail, remainingN, remainingK, currCombination)
          }
        }
      }

      findCombinations(List(1, 2, 3, 4, 5, 6, 7, 8, 9), n, k, Nil).filter(_.nonEmpty)
    }
  }
}
