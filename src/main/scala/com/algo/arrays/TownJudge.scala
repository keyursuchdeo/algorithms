package com.algo.arrays

import scala.collection.mutable

object TownJudge extends App {

//  val N = 2
//  val trust = Array(Array(1, 2))
  val N = 4
  val trust = Array(Array(1, 3), Array(1, 4), Array(2, 3), Array(2, 4), Array(4, 3))
  val res = Solution.findJudge(N, trust)
  println(res)

  object Solution {
    def findJudge(N: Int, trust: Array[Array[Int]]): Int = {
      val possibleJudges: Array[Int] = new Array[Int](N)

      @scala.annotation.tailrec
      def findPossibleJudge(index: Int, peopleTrustMapping: mutable.Map[Int, Set[Int]]): (Array[Int], mutable.Map[Int, Set[Int]])  = {
        if(index == trust.length) {
          (possibleJudges.zipWithIndex.filter(_._1 == 0).map(_._2 + 1), peopleTrustMapping)
        } else {
          possibleJudges(trust(index)(0) - 1) = trust(index)(0)
          findPossibleJudge(index + 1, peopleTrustMapping +
            (trust(index)(0) -> (peopleTrustMapping.getOrElse(trust(index)(0), Set()) + trust(index)(1))))
        }
      }

      val (judges, trustMapping) = findPossibleJudge(0, mutable.Map())
      println(possibleJudges.mkString(","))
      println(judges.mkString(","))
      println(trustMapping)
      if(judges.length != 1) {
        -1
      } else {
        if (trustMapping.values.exists(!_.contains(judges.head))) -1 else judges.head
      }
    }
  }
}
