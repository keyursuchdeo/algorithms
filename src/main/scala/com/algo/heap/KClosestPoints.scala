package com.algo.heap

import scala.collection.mutable

object KClosestPoints extends App {



  object Solution {
    def kClosest(points: Array[Array[Int]], K: Int): Array[Array[Int]] = {
      object MinOrder extends Ordering[Array[Int]] {
        def compare(x: Array[Int], y: Array[Int]): Int = {
          distanceFromOrigin(y) compare distanceFromOrigin(x)
        }

        private def distanceFromOrigin(point: Array[Int]): Double = {
          Math.sqrt(point(0) * point(0) + point(1) * point(1))
        }
      }

      val queue: mutable.PriorityQueue[Array[Int]] = scala.collection.mutable.PriorityQueue.empty(MinOrder)

      @scala.annotation.tailrec
      def fillQueue(index: Int): Unit = {
        if(index == points.length) {
          ()
        } else {
          queue.enqueue(points(index))
          fillQueue(index + 1)
        }
      }

      @scala.annotation.tailrec
      def getKPoints(index: Int, seq: Seq[Array[Int]]): Seq[Array[Int]] = {
        if(index == K) {
          seq
        } else {
          getKPoints(index + 1, queue.dequeue() +: seq)
        }
      }

      fillQueue(0)
      getKPoints(0, Nil).toArray
    }
  }
}
