package com.algo.arrays

object RedundantConnection extends App {
  object Solution {
    def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] = {

      var visitedNodes = Set[Int]()
      val adjList = Array.fill[Seq[Int]](edges.length + 1)(Nil)

      @scala.annotation.tailrec
      def find(index: Int): Array[Int] = {
        if (index == edges.length) {
          Array.empty
        } else {
          visitedNodes = visitedNodes.empty
          val Array(from, to) = edges(index)
          if (dfsStarting(from, to)) {
            edges(index)
          } else {
            adjList(from) = to +: adjList(from)
            adjList(to) = from +: adjList(to)
            find(index + 1)
          }

        }
      }

      def dfsStarting(from: Int, to: Int): Boolean = {
        if(!visitedNodes.contains(from)) {
          visitedNodes = visitedNodes + from
          if(from == to) {
            true
          } else {
            adjList(from).exists(neighbour => {
              dfsStarting(neighbour, to)
            })
          }
        } else {
          false
        }
      }

      find(0)
    }
  }
}
