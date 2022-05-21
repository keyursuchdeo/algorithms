package com.algo.arrays

import scala.collection.mutable

object SwimInRisingWater extends App {
  object Solution {
    def swimInWater(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)
      val visited = Array.ofDim[Boolean](rows, cols)

      object MinOrder extends Ordering[(Int, Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          val (xRow, xCol) = x
          val (yRow, yCol) = y
          grid(yRow)(yCol) compare grid(xRow)(xCol)
        }
      }
      val minHeap: mutable.PriorityQueue[(Int, Int)] = scala.collection.mutable.PriorityQueue.empty(MinOrder)

      def isValid(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row >= 0 && row < rows && col >= 0 && col < cols
      }

      def neighboursOf(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter(isValid)
      }

      @scala.annotation.tailrec
      def find(ans: Int): Int = {
        if(minHeap.isEmpty) {
          ans
        } else {
          val (row, col) = minHeap.dequeue()
          if(row == rows - 1 && col == cols - 1) {
            find(Math.max(ans, grid(row)(col)))
          } else {
            val neighbours = neighboursOf(row, col)
            neighbours.foreach(neighbour => {
              val (nRow, nCol) = neighbour
              if(!visited(nRow)(nCol)) {
                visited(nRow)(nCol) = true
                minHeap.enqueue((nRow, nCol))
              }
            })
            find(Math.max(ans, grid(row)(col)))
          }
        }
      }

      minHeap.enqueue((0, 0))
      visited(0)(0) = true
      find(0)
    }
  }

}
