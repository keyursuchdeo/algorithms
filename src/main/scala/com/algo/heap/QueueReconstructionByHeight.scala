package com.algo.heap

import scala.collection.mutable

object QueueReconstructionByHeight extends App {
  object Solution {
    def reconstructQueue(people: Array[Array[Int]]): Array[Array[Int]] = {
      implicit object MaxOrder extends Ordering[Array[Int]] {
        def compare(x: Array[Int], y: Array[Int]): Int = {
          val a = y.head compare x.head
          if (a == 0) {
            x(1) compare y(1)
          } else {
            a
          }
        }
      }

      val sortedPeopleByHeight: Array[Array[Int]] = people.sorted

      @scala.annotation.tailrec
      def reconstruct(index: Int, sortedQueue: Seq[Array[Int]]): Array[Array[Int]] = {
        if(index == people.length) {
          sortedQueue.toArray
        } else {
          reconstruct(index + 1, insertAt(index, sortedQueue))
        }
      }

      def insertAt(index: Int, sortedQueue: Seq[Array[Int]]): Seq[Array[Int]] = {
        val (before, after) = sortedQueue.splitAt(sortedPeopleByHeight(index)(1))
        before ++ Seq(sortedPeopleByHeight(index)) ++ after
      }

      println(sortedPeopleByHeight.map(_.mkString(",")).mkString("|"))
      reconstruct(0, Nil)

    }
  }
}
