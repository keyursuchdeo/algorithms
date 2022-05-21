package com.algo.tree

object MinHeightTree extends App {

  object Solution {
    def findMinHeightTrees(n: Int, edges: Array[Array[Int]]): List[Int] = {
      lazy val adjList = Array.fill[Set[Int]](n)(Set())

      @scala.annotation.tailrec
      def fillAdjList(index: Int): Unit = {
        if (index == edges.length) {
          ()
        } else {
          val Array(node1, node2) = edges(index)
          adjList(node1) = adjList(node1) + node2
          adjList(node2) = adjList(node2) + node1
          fillAdjList(index + 1)
        }
      }

      @scala.annotation.tailrec
      def prepLeaves(index: Int, leaves: Seq[Int]): Vector[Int] = {
        if (index == adjList.length) {
          leaves.toVector
        } else {
          if (adjList(index).size == 1) {
            prepLeaves(index + 1, index +: leaves)
          } else {
            prepLeaves(index + 1, leaves)
          }
        }
      }

      def cutLeavesAndFindNeighbouringOnes(leaves: Vector[Int]): Vector[Int] = {
        leaves.map(leaf => {
          val neighbour = adjList(leaf).head
          adjList(leaf) = Set()
          adjList(neighbour) = adjList(neighbour) - leaf
          if(adjList(neighbour).size == 1) neighbour else -1
        }).filter(_ > -1)
      }

      @scala.annotation.tailrec
      def find(remainingNodes: Int, leaves: Vector[Int]): Seq[Int] = {
        if (remainingNodes <= 2) {
          leaves
        } else {
          val neighbouringLeaves = cutLeavesAndFindNeighbouringOnes(leaves)
          find(remainingNodes - leaves.size, neighbouringLeaves)
        }
      }

      if(n <= 2) {
        (0 until n).toList
      } else {
        fillAdjList(0)
        find(n, prepLeaves(0, Seq())).toList
      }
    }
  }

}
