package com.algo.dfs

import scala.collection.mutable

object CheapestFlightKStops extends App {
  val nodes = 3
  val edges: Array[Array[Int]] = Array(Array(0, 1, 100), Array(1, 2, 200), Array(0, 2, 500))
  val s = 0
  val d = 2
  val k = 1

  val res = Solution.findCheapestPrice(nodes, edges, s, d, k)
  println(res)

  object Solution {
    def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, K: Int): Int = {
      val adjList = Array.fill[Seq[Int]](n)(Nil)
      val adjListWithCost = Array.fill[Seq[(Int, Int)]](n)(Nil)
      val costsToDestFromNode = Array.fill[Int](n)(-1)
      val stopsToDestFromNode = Array.fill[Int](n)(Int.MaxValue)
      val queue = new mutable.Queue[Int]()

      @scala.annotation.tailrec
      def prepAdjList(index: Int): Unit = {
        if(index == flights.length) {
          ()
        } else {
          val Array(from, to, price) = flights(index)
          adjList(to) = from +: adjList(to)
          adjListWithCost(to) = (from, price) +: adjListWithCost(to)
          prepAdjList(index + 1)
        }
      }

      @scala.annotation.tailrec
      def find(): Unit = {
        if(queue.isEmpty) {
          ()
        } else {
          val node = queue.dequeue()
          val neighbours = adjListWithCost(node)
          neighbours.foreach(neighbourCost => {
            val (neighbour, cost) = neighbourCost
            if(costsToDestFromNode(neighbour) == -1) {
              costsToDestFromNode(neighbour) = cost + costsToDestFromNode(node)
              stopsToDestFromNode(neighbour) = 1 + stopsToDestFromNode(node)
              queue.enqueue(neighbour)
            } else {
              if(costsToDestFromNode(neighbour) < cost + costsToDestFromNode(node) ||
                1 + stopsToDestFromNode(node) > K) {
                ()
              } else {
                costsToDestFromNode(neighbour) = cost + costsToDestFromNode(node)
                stopsToDestFromNode(neighbour) = 1 + stopsToDestFromNode(node)
                queue.enqueue(neighbour)
              }
            }
          })
          find()
        }
      }

      prepAdjList(0)
      queue.enqueue(dst)
      costsToDestFromNode(dst) = 0
      stopsToDestFromNode(dst) = -1
      find()
      if(stopsToDestFromNode(src) > K) -1 else costsToDestFromNode(src)
    }
  }
}
