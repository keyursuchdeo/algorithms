package com.algo.backtracking

import scala.collection.mutable

object RemoveInvalidParentheses extends App {

  Solution.removeInvalidParentheses("()((((((()l(")

  object Solution {
    def removeInvalidParentheses(s: String): List[String] = {
      val out = if(balancedP(s)) List(s) else findSolution(s)
      val distinctOut = out.distinct
      println(distinctOut)
      println(distinctOut.size)
      distinctOut
    }

    private def findSolution(s: String): List[String] = {
      val queue = new mutable.Queue[QueuedElement]()

      @scala.annotation.tailrec
      def find(index: Int, totalNumOfRemovals: Int, minNumOfRemovalsForBalancedP: Int, strings: List[String], string: String): (List[String], Int, Int) = {
        if (index == string.length) {
          (strings, totalNumOfRemovals + 1, minNumOfRemovalsForBalancedP)
        } else {
          val afterRemoval = removeCharAtIndex(index, string)
          if(balancedP(afterRemoval)) {
            find(index + 1, totalNumOfRemovals, totalNumOfRemovals + 1, afterRemoval +: strings, string)
          } else {
            queue.enqueue(QueuedElement(afterRemoval, totalNumOfRemovals + 1))
            find(index + 1, totalNumOfRemovals, minNumOfRemovalsForBalancedP, strings, string)
          }
        }
      }

      @scala.annotation.tailrec
      def findTillQueueIsEmpty(allStrings: List[String], totaNumOfRemovalsYet: Int, minNumOfRemovalsYet: Int): List[String] = {
        if(queue.isEmpty) {
          allStrings
        } else {
          val queuedElement = queue.dequeue()
          if(queuedElement.numOfRemovals < minNumOfRemovalsYet) {
            val (stringsTillNow, totalRemovals, minRemovals) =
              find(0, queuedElement.numOfRemovals, minNumOfRemovalsForBalancedP = minNumOfRemovalsYet, strings = allStrings, queuedElement.s)
            findTillQueueIsEmpty(stringsTillNow, totalRemovals, minRemovals)
          } else {
            findTillQueueIsEmpty(allStrings, totaNumOfRemovalsYet, minNumOfRemovalsYet)
          }
        }
      }

      val (strings, totaNumOfRemovals, minNumOfApprovals) = find(0, 0, Int.MaxValue, Nil, s)
      findTillQueueIsEmpty(strings, totaNumOfRemovals, minNumOfApprovals)
    }

    case class QueuedElement(s: String, numOfRemovals: Int)

    private def removeCharAtIndex(i: Int, s: String): String = {
      val (before, after) = s.splitAt(i)
      val afterRemoval = after.substring(1)
      s"$before$afterRemoval"
    }

    private def balancedP(s: String): Boolean = {
      @scala.annotation.tailrec
      def checkIfBalanced(index: Int, openPCount: Int, closePCount: Int): Boolean = {
        if(index == s.length) {
          openPCount == closePCount
        } else {
          if (s(index) == '(') {
            checkIfBalanced(index + 1, openPCount + 1, closePCount)
          } else if (s(index) == ')') {
            if(closePCount + 1 <= openPCount) {
              checkIfBalanced(index + 1, openPCount, closePCount + 1)
            } else {
              false
            }
          } else {
            checkIfBalanced(index + 1, openPCount, closePCount)
          }
        }
      }
      if(s.isEmpty || !(s.contains('(') || s.contains(')'))) true else {
        checkIfBalanced(0, 0, 0)
      }
    }
  }
}
