package com.algo.bfs

import scala.collection.mutable

object ShortestPathInBinaryMatrix extends App {
  object Solution {
    def shortestPathBinaryMatrix(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)


      def isValidAndUnvisited(row: Int, col: Int): Boolean =
        row >= 0 && row < rows && col >= 0 && col < cols && grid(row)(col) == 0

      def findUnvisitedNeighbours(row: Int, col: Int) = {
        Seq(
          (row, col + 1),
          (row + 1, col),
          (row, col - 1),
          (row - 1, col),
          (row + 1, col + 1),
          (row - 1, col + 1),
          (row - 1, col - 1),
          (row + 1, col - 1)
        ).filter(cell => {
          val (r, c) = cell
          isValidAndUnvisited(r, c)
        })
      }

      def isDestination(row: Int, col: Int): Boolean = {
        row == rows - 1 && col == cols - 1
      }

      @scala.annotation.tailrec
      def find(queue: mutable.Queue[(Int, Int, Int)]): Int = {
        if(queue.isEmpty) {
          -1
        } else {
          val (row, col, steps) = queue.dequeue()
          if(isDestination(row, col)) {
            steps + 1
          } else {
            val unvisitedNeighbours = findUnvisitedNeighbours(row, col)
            unvisitedNeighbours.foreach(cell => {
              val (r, c) = cell
              grid(r)(c) = 1
              queue.enqueue((r, c, steps + 1))
            })
            find(queue)
          }
        }
      }

      if(grid(0)(0) == 1) {
        -1
      }  else {
        val q = new mutable.Queue[(Int, Int, Int)]()
        grid(0)(0) = 1
        q.enqueue((0, 0, 0))
        find(q)
      }
    }
  }
}
