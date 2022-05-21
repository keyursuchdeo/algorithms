package com.algo.dfs

object SumDistancesInTree extends App {
  object Solution {
    def sumOfDistancesInTree(n: Int, edges: Array[Array[Int]]): Array[Int] = {
      val adjList: Array[Seq[Int]] = Array.fill[Seq[Int]](n)(Nil)
      def fillAdjList(index: Int): Unit = {
        if (index == edges.length) {
          ()
        } else {
          val Array(p1, p2) = edges(index)
          adjList(p1) = p2 +: adjList(p1)
          adjList(p2) = p1 +: adjList(p2)
        }
      }
    }
  }
}
