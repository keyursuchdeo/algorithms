package com.algo.bfs

import scala.collection.mutable

object OpenLock extends App {

  val d = Array("0201","0101","0102","1212","2002")
  val t = "0202"
  val res = Solution.openLock(d, t)
  println(res)

  object Solution {
    def openLock(deadends: Array[String], target: String): Int = {
      var visited: Set[String] = Set[String]()
      val deads: Set[String] = deadends.toSet

      def findNeighbours(position: String): Seq[String] = {
        val chars = position.toCharArray.map(_.asDigit)
        @scala.annotation.tailrec
        def find(index: Int, allNeighbours: Seq[String]): Seq[String] = {
          if(index > 3) {
            allNeighbours
          } else {
            val positiveReplacementDigit: Int =
            if(chars(index) + 1 > 9) {
              0
            } else {
              (chars(index) + 1)
            }
            val newChars1 = chars.clone()
            newChars1(index) = positiveReplacementDigit
            val n1 = newChars1.mkString

            val negativeReplacementDigit: Int =
              if(chars(index) - 1 < 0) {
                9
              } else {
                (chars(index) - 1)
              }
            val newChars2 = chars.clone()
            newChars2(index) = negativeReplacementDigit
            val n2 = newChars2.mkString

            find(index + 1, Seq(n1, n2) ++ allNeighbours)
          }
        }

        find(0, Nil)
      }

      @scala.annotation.tailrec
      def open(posQueue: mutable.Queue[(String, Int)]): Int = {
        println(posQueue)
        if(posQueue.isEmpty) {
          -1
        } else {
          val (pos, step) = posQueue.dequeue()
          if (deads.contains(pos)) {
            open(posQueue)
          } else if (pos == target) {
            step
          } else {
            val neighbours = findNeighbours(pos)
            neighbours.foreach(neighbour => {
              if(!visited.contains(neighbour)) {
                visited = visited + neighbour
                posQueue.enqueue((neighbour, step + 1))
              }
            })
            open(posQueue)
          }
        }
      }

      val queue = new mutable.Queue[(String, Int)]()
      queue.enqueue(("0000", 0))
      visited = visited + "0000"
      open(queue)
    }
  }

}
