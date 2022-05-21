package com.algo.dfs

object IsGraphBipartite extends App {

  object Solution {
    def isBipartite(graph: Array[Array[Int]]): Boolean = {
      val nodeSets = Array.fill[Int](graph.length)(-1)
      var isBipartite = true


      def checkFrom(index: Int): Unit = {
        val neighbours = graph(index)
        neighbours.foreach(n => {
          if (nodeSets(n) < 0) {
            nodeSets(n) = nodeSets(index) ^ 1
            checkFrom(n)
          } else {
            if (nodeSets(n) == nodeSets(index)) {
              isBipartite = false
            }
          }
        })
      }

      @scala.annotation.tailrec
      def check(index: Int): Boolean = {
        if(index == graph.length) {
          true
        } else {
          if(nodeSets(index) == -1) {
            nodeSets(index) = 0
            checkFrom(index)
            if(isBipartite) {
              check(index + 1)
            } else {
              false
            }
          } else {
            check(index + 1)
          }
        }
      }

      check(0)
    }
  }

}
