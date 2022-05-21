package com.algo.backtracking

object CombinationSum extends App {

  val a = Array(21,46,35,20,44,31,29,23,45,37,33,34,39,42,24,40,41,26,22,38,36,27,25,49,48,43)
  val t = 71

  val res = Solution.combinationSum(a, t)
  println(res)

  object Solution {
    def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {

      val candidatesWithIndex: Array[(Int, Int)] = candidates.zipWithIndex

      @scala.annotation.tailrec
      def find(index: Int, sums: List[List[Int]]): List[List[Int]] = {
        if(index == candidates.length) {
          sums
        } else {
          find(index + 1, sums ++ findStarting(index))
        }
      }

      case class CurrState(currSum: Int, remainingCandidateNums: Seq[(Int, Int)], currSumCandidateNums: List[Int])

      def findStarting(startingIndex: Int): List[List[Int]] = {
        @scala.annotation.tailrec
        def find(candidateNumIndex: Int, candidateNum: Int, sums: List[List[Int]], currSum: Int, currSumCandidateNums: List[Int], stack: Seq[CurrState]): List[List[Int]] = {
          if(currSum == target) {
            if(stack.nonEmpty) {
              val poppedState = stack.head
//              if(poppedState.remainingCandidateNums.tail.nonEmpty) {
//                val updatedPoppedState = poppedState.copy(remainingCandidateNums = poppedState.remainingCandidateNums.tail)
//                find(poppedState.remainingCandidateNums.head._2, poppedState.remainingCandidateNums.head._1, currSumCandidateNums +: sums, poppedState.currSum, poppedState.currSumCandidateNums, updatedPoppedState +: stack.tail)
//              } else {
//                find(poppedState.remainingCandidateNums.head._2, poppedState.remainingCandidateNums.head._1, currSumCandidateNums +: sums, poppedState.currSum, poppedState.currSumCandidateNums, stack.tail)
//              }
              find(poppedState.remainingCandidateNums.head._2, poppedState.remainingCandidateNums.head._1, currSumCandidateNums +: sums, poppedState.currSum, poppedState.currSumCandidateNums, stack.tail)

            } else {
              currSumCandidateNums +: sums
            }
          } else if (currSum > target) {
            if(stack.nonEmpty) {
              val poppedState = stack.head
              println(poppedState)
//              if(poppedState.remainingCandidateNums.tail.nonEmpty) {
//                val updatedPoppedState = poppedState.copy(remainingCandidateNums = poppedState.remainingCandidateNums.tail)
//                find(poppedState.remainingCandidateNums.head._2, poppedState.remainingCandidateNums.head._1, sums, poppedState.currSum, poppedState.currSumCandidateNums, updatedPoppedState +: stack.tail)
//              } else {
//                find(poppedState.remainingCandidateNums.head._2, poppedState.remainingCandidateNums.head._1, sums, poppedState.currSum, poppedState.currSumCandidateNums, stack.tail)
//              }
              find(poppedState.remainingCandidateNums.head._2, poppedState.remainingCandidateNums.head._1, sums, poppedState.currSum, poppedState.currSumCandidateNums, stack.tail)
            } else {
              sums
            }
          } else {
            val remainingCandidateNums = candidatesWithIndex.drop(candidateNumIndex + 1)
            if(remainingCandidateNums.nonEmpty) {
              val currState = CurrState(currSum, remainingCandidateNums, currSumCandidateNums)
              find(candidateNumIndex, candidateNum, sums, currSum + candidateNum, candidateNum +: currSumCandidateNums, currState +: stack)
            } else {
              find(candidateNumIndex, candidateNum, sums, currSum + candidateNum, candidateNum +: currSumCandidateNums, stack)
            }
          }
        }
        find(startingIndex, candidates(startingIndex), Nil, candidates(startingIndex), List(candidates(startingIndex)), Nil)
      }

      find(0, Nil)
    }
  }
}
