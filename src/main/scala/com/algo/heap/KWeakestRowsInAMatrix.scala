package com.algo.heap

object KWeakestRowsInAMatrix extends App {
  object Solution {
    def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] = {
      val rows = mat.length
      val cols = mat.headOption.map(_.length).getOrElse(0)
      val output = new Array[Int](k)

      object WeakRowOrder extends Ordering[(Int, Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          val (xOneCount, xIndex) = x
          val (yOneCount, yIndex) = y
          if(xOneCount == yOneCount) {
            yIndex compare xIndex
          } else {
            yOneCount compare xOneCount
          }
        }
      }

      val rowQueue = scala.collection.mutable.PriorityQueue.empty(WeakRowOrder)

      def countOnes(row: Int) = {
        @scala.annotation.tailrec
        def count(col: Int, total: Int): Int = {
          if(col == cols || mat(row)(col) == 0) {
            total
          } else {
            count(col + 1, total + 1)
          }
        }
        count(0, 0)
      }

      @scala.annotation.tailrec
      def fillQueue(index: Int): Unit = {
        if(index == rows) {
          ()
        } else {
          rowQueue.enqueue((countOnes(index), index))
          fillQueue(index + 1)
        }
      }

      @scala.annotation.tailrec
      def kWeakest(currK: Int): Unit = {
        if(currK == k) {
          ()
        } else {
          output(currK) = rowQueue.dequeue()._2
          kWeakest(currK + 1)
        }
      }

      fillQueue(0)
      kWeakest(0)
      output
    }
  }
}
