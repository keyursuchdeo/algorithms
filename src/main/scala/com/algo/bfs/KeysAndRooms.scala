package com.algo.bfs

import scala.collection.mutable

object KeysAndRooms extends App {
  object Solution {
    def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
      var visitedRooms = Set[Int]()
      var numOfVisitedRooms = 0

      @scala.annotation.tailrec
      def visitRooms(queue: mutable.Queue[Int]): Unit = {
        if(queue.isEmpty) {
          ()
        } else {
          val room = queue.dequeue()
          if(visitedRooms.contains(room)) {
            visitRooms(queue)
          } else {
            visitedRooms = visitedRooms + room
            numOfVisitedRooms = numOfVisitedRooms + 1
            rooms(room).foreach(key => queue.enqueue(key))
            visitRooms(queue)
          }
        }
      }

      val q = mutable.Queue[Int]()
      q.enqueue(0)
      visitRooms(q)
      numOfVisitedRooms == rooms.length
    }
  }
}
