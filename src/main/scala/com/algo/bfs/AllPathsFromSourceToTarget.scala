package com.algo.bfs

object AllPathsFromSourceToTarget extends App {

  object Solution {
    def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
      val destination = graph.length - 1

      def findPaths(currSources: Array[Int], currPath: List[Int], paths: List[List[Int]]): List[List[Int]] = {
        if (currSources.isEmpty) {
          if (currPath.head == destination) {
            currPath.reverse +: paths
          } else {
            paths
          }
        } else {
          currSources.flatMap(source => {
            findPaths(graph(source), source +: currPath, paths)
          }).toList
        }
      }

      findPaths(Array(0), Nil, Nil)
    }
  }

}
