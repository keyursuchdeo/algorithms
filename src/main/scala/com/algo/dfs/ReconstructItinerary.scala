package com.algo.dfs

import scala.collection.mutable

object ReconstructItinerary extends App {

  val t = List(List("JFK","SFO"), List("JFK","ATL"), List("SFO","ATL"), List("ATL","JFK"), List("ATL","SFO"))
  val res = Solution.findItinerary(t)
  println(res)

  object Solution {
    private lazy val Source = "JFK"

    def findItinerary(tickets: List[List[String]]): List[String] = {

      object MinOrder extends Ordering[String] {
        override def compare(x: String, y: String): Int = y compare x
      }

      def buildPriorityQueue(): mutable.PriorityQueue[String] = {
        scala.collection.mutable.PriorityQueue.empty(MinOrder)
      }

      var adjList: mutable.Map[String, mutable.PriorityQueue[String]] = mutable.Map()
      var itinerary = Seq[String]()

      @scala.annotation.tailrec
      def prepAdjList(currTickets: List[List[String]]): Unit = {
        if (currTickets.isEmpty) {
          ()
        } else {
          val List(from, to) = currTickets.head
          val destinations: mutable.PriorityQueue[String] = adjList.get(from) match {
            case Some(destinations) => destinations
            case None => buildPriorityQueue()
          }
          destinations.enqueue(to)
          adjList = adjList + (from -> destinations)
          prepAdjList(currTickets.tail)

        }
      }

      def dfs(from: String): Unit = {
        adjList.get(from) match {
          case Some(destinations) if destinations.nonEmpty =>
            while(destinations.nonEmpty) {
              dfs(destinations.dequeue())
            }
//            destinations.dequeueAll.foreach(dfs)
            itinerary = from +: itinerary
          case _ =>
            itinerary = from +: itinerary
        }
      }

      prepAdjList(tickets)
      dfs(Source)
      itinerary.toList
    }
  }

}
