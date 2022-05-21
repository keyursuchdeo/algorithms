package com.algo.backtracking

import scala.collection.mutable

object SumOfSubsets extends App {

  val input = Array(5, 10, 12, 13, 15, 18)
  val desiredSum = 30

  Solution.subsets(input, desiredSum)

  object Solution {
    def subsets(a: Array[Int], desiredSum: Int): Seq[Seq[Int]] = {
      val stack: mutable.Stack[IndexInclExcl] = new mutable.Stack[IndexInclExcl]
      val remainingSum = a.sum
      val inclA = new Array[Int](a.length)
      val b: Seq[Seq[Int]] = find(a, 0, 0, remainingSum, desiredSum, stack, inclA, Nil)
      b.foreach(c => println(c.mkString(",")))
      b
    }

    @scala.annotation.tailrec
    def find(a: Array[Int], index: Int, inclSum: Int, remainingSum: Int, desiredSum: Int, stack: mutable.Stack[IndexInclExcl], inclA: Array[Int], allInclAs: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      if(inclSum == desiredSum) {
        if(stack.isEmpty) {
          inclA.toSeq +: allInclAs
        } else {
          val b = inclA.toSeq +: allInclAs
          println("------------")
          b.foreach(c => println(c.mkString(",")))
          println("------------")
          val backtrackToIndex = stack.pop()
          inclA(backtrackToIndex.index) = 0
          find(
            a,
            backtrackToIndex.index + 1,
            backtrackToIndex.inclSum,
            backtrackToIndex.remainingSum - a(backtrackToIndex.index),
            desiredSum,
            stack,
            inclA,
            b)
        }
      } else if(index < a.length && inclSum + a(index) > desiredSum) {
        find(a, index + 1, inclSum, remainingSum - a(index), desiredSum, stack, inclA, allInclAs)
      } else if (inclSum + remainingSum < desiredSum) {
        if(stack.isEmpty) {
          println("*************")
          allInclAs.foreach(c => println(c.mkString(",")))
          println("*************")
          allInclAs
        } else {
          val backtrackToIndex = stack.pop()
          inclA(backtrackToIndex.index) = 0
          find(
            a,
            backtrackToIndex.index + 1,
            backtrackToIndex.inclSum,
            backtrackToIndex.remainingSum - a(backtrackToIndex.index),
            desiredSum,
            stack,
            inclA,
            allInclAs)
        }
      } else {
        stack.push(IndexInclExcl(index, inclSum, remainingSum))
        inclA(index) = 1
        find(a, index + 1, inclSum + a(index), remainingSum - a(index), desiredSum, stack, inclA, allInclAs)
      }
    }

    case class IndexInclExcl(index: Int, inclSum: Int, remainingSum: Int)
  }
}
