package com.algo.bfs

object JumpGameIV extends App {
  object Solution {
    def minJumps(arr: Array[Int]): Int = {

      val visited: Array[Boolean] = new Array[Boolean](arr.length)

      @scala.annotation.tailrec
      def prepAdjList(index: Int, map: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
        if(index == arr.length) {
          map
        } else {
          map.get(arr(index)) match {
            case Some(value) =>
              prepAdjList(index + 1, map + (arr(index) -> (value + index)))
            case None =>
              prepAdjList(index + 1, map + (arr(index) -> Set(index)))
          }
        }
      }

      var adjList: Map[Int, Set[Int]] = prepAdjList(0, Map())

      def findNeighbours(index: Int): Set[Int] = {
        if(index == arr.length - 1) {
          if(visited(arr.length - 2)) {
            adjList(arr(index))
          } else {
            adjList(arr(index)) + (arr.length - 2)
          }
        } else if (index == 0) {
          if(visited(1)) {
            adjList(arr(index))
          } else {
            adjList(arr(index)) + 1
          }
        } else {
          if(visited(index - 1) && visited(index + 1)) {
            adjList(arr(index))
          } else if(visited(index - 1)) {
            adjList(arr(index)) + (index + 1)
          } else if (visited(index + 1)) {
            adjList(arr(index)) + (index - 1)
          } else {
            (adjList(arr(index)) + (index - 1)) + (index + 1)
          }
        }
      }

      @scala.annotation.tailrec
      def bfsFrom(set: Set[Int], stepCount: Int): Int = {
        if(set.contains(arr.length - 1)) {
          stepCount
        } else {
          val updatedSet: Set[Int] =
          set.flatMap(index => {
            visited(index) = true
            adjList = adjList + (arr(index) -> (adjList.getOrElse(arr(index), Set()) - index))
            val o = findNeighbours(index)
            adjList = adjList - arr(index)
            o
          })
          bfsFrom(updatedSet, stepCount + 1)
        }
      }

      bfsFrom(Set(0), 0)

    }
  }
}
