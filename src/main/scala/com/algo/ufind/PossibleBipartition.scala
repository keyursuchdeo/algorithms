package com.algo.ufind

object PossibleBipartition extends App {

//  val n = 4
//  val dislikes = Array(Array(1, 2), Array(1, 3), Array(2, 4))

//  val n = 3
//  val dislikes = Array(Array(1, 2), Array(2, 3), Array(1, 3))

//  val n = 5
//  val dislikes = Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5), Array(1, 5))

  val n = 6
  val dislikes = Array(Array(1,2),Array(1,3),Array(2,4),Array(4,5),Array(5,6),Array(3,6))
  val res = Solution.possibleBipartition(n, dislikes)
  println(res)

  object Solution {
    def possibleBipartition(N: Int, dislikes: Array[Array[Int]]): Boolean = {

      def find(index: Int, group1: Set[Int], group2: Set[Int]): Boolean = {
        if(index == dislikes.length){
          true
        } else {
          val Array(p1, p2) = dislikes(index)
          (findCurrGroup(p1, group1, group2), findCurrGroup(p2, group1, group2)) match {
            case (None, None) =>
              find(index + 1, group1 + p1, group2 + p2) || find(index + 1, group1 + p2, group2 + p1)
            case (Some(g1), None) if g1 == group1 =>
              find(index + 1, group1, group2 + p2)
            case (Some(g1), None) if g1 == group2 =>
              find(index + 1, group1 + p2, group2)
            case (None, Some(g1)) if g1 == group1 =>
              find(index + 1, group1, group2 + p1)
            case (None, Some(g1)) if g1 == group2 =>
              find(index + 1, group1 + p1, group2)
            case (Some(g1), Some(g2)) if g1 == g2 =>
              false
            case (Some(_), Some(_)) =>
              find(index + 1, group1, group2)
          }
        }
      }

      def findCurrGroup(num: Int, group1: Set[Int], group2: Set[Int]): Option[Set[Int]] = {
        if (group1.contains(num)) {
          Some(group1)
        } else if (group2.contains(num)) {
          Some(group2)
        } else {
          None
        }
      }

      find(0, Set(), Set())


    }
  }
}
