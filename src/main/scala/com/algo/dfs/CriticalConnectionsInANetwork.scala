package com.algo.dfs

object CriticalConnectionsInANetwork extends App {
  object Solution {
    def criticalConnections(n: Int, connections: List[List[Int]]): List[List[Int]] = {
      val adjList = Array.fill[Seq[Int]](n)(Nil)
      val startingTime = new Array[Int](n)
      val lowLink = new Array[Int](n)
      var output: List[List[Int]] = Nil
      var time = 1
      @scala.annotation.tailrec
      def prepAdjList(currConnections: List[List[Int]]): Unit = {
        if(currConnections.isEmpty) {
          ()
        } else {
          val List(from, to) = currConnections.head
          adjList(from) = to +: adjList(from)
          adjList(to) = from +: adjList(to)
          prepAdjList(currConnections.tail)
        }
      }

      def dfsFrom(node: Int, parent: Int): Unit = {
        startingTime(node) = time
        lowLink(node) = time
        time = time + 1
        val neighbours = adjList(node)
        neighbours.foreach(neighbour => {
          if(startingTime(neighbour) == 0) {
            dfsFrom(neighbour, node)
            lowLink(node) = Math.min(lowLink(neighbour), lowLink(node))
          } else {
            if(neighbour != parent) {
              lowLink(node) = Math.min(lowLink(node), startingTime(neighbour))
            }
          }
          if(lowLink(neighbour) > startingTime(node)) {
            output = List(neighbour, node) +: output
          }
        })
      }

      prepAdjList(connections)
      dfsFrom(0, -1)
      output
    }
  }
}
