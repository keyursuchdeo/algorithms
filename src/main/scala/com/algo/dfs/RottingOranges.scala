package com.algo.dfs

import scala.collection.mutable

object RottingOranges extends App {

  val g = Array.ofDim[Int](3, 3)
  g(0) = Array(2, 1, 1)
  g(1) = Array(1, 1, 0)
  g(2) = Array(0, 1, 1)

  val res = Solution.orangesRotting(g)
  println(res)

  object Solution {
    def orangesRotting(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)
      val visitedOranges: Array[Array[Int]] = Array.fill[Int](rows, cols)(-1)

      @scala.annotation.tailrec
      def calculate(row: Int, col: Int): Unit = {
        if (row == rows) {
          ()
        } else if (col == cols) {
          calculate(row + 1, 0)
        } else if (grid(row)(col) == 2 && visitedOranges(row)(col) == -1) {
          val queue = new mutable.Queue[(Int, Int, Int)]()
          visitedOranges(row)(col) = 0
          queue.enqueue((row, col, 0))
          bfs(queue)
          calculate(row, col + 1)
        } else if (grid(row)(col) == 0 && visitedOranges(row)(col) == -1) {
          visitedOranges(row)(col) = 0
          calculate(row, col + 1)
        } else {
          calculate(row, col + 1)
        }
      }


      @scala.annotation.tailrec
      def bfs(queue: mutable.Queue[(Int, Int, Int)]): Unit = {
        if (queue.isEmpty) {
          ()
        } else {
          val (row, col, time) = queue.dequeue()
          neighbours(row, col).foreach(neighbour => {
            val (nRow, nCol) = neighbour
            if (nRow >= 0 && nRow < rows && nCol >= 0 && nCol < cols &&
              grid(nRow)(nCol) != 2) {
              if(grid(nRow)(nCol) == 1) {
                if(visitedOranges(nRow)(nCol) == -1) {
                  visitedOranges(nRow)(nCol) = time + 1
                  queue.enqueue((nRow, nCol, time + 1))
                } else {
                  if(time + 1 < visitedOranges(nRow)(nCol)) {
                    visitedOranges(nRow)(nCol) = time + 1
                    queue.enqueue((nRow, nCol, time + 1))
                  }
                }
              } else {
                visitedOranges(nRow)(nCol) = 0
              }
            }
          })
          bfs(queue)
        }
      }

      def neighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1))
      }

      calculate(0, 0)
      val flattenedArray = visitedOranges.flatten
      if(flattenedArray.isEmpty) {
        0
      } else {
        val max = flattenedArray.max
        val min = flattenedArray.min
        if(min == -1) {
          -1
        } else {
          max
        }
      }
    }
  }

}
