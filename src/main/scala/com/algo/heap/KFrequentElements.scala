package com.algo.heap

import scala.collection.mutable

object KFrequentElements extends App {
  object Solution {
    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
      object FreqOrdering extends Ordering[(Int, Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          x._2 compare y._2
        }
      }

      val queue: mutable.PriorityQueue[(Int, Int)] = mutable.PriorityQueue.empty(FreqOrdering)
      @scala.annotation.tailrec
      def populateFreq(index: Int, map: Map[Int, Int]): Map[Int, Int] = {
        if(index == nums.length) {
          map
        } else {
          map.get(nums(index)) match {
            case Some(count) =>
              populateFreq(index + 1, map + (nums(index) -> (count + 1)))
            case None =>
              populateFreq(index + 1, map + (nums(index) -> 1))
          }
        }
      }

      @scala.annotation.tailrec
      def find(elements: Seq[Int]): Seq[Int] = {
        if(queue.isEmpty){
          elements
        } else {
          val element = queue.dequeue()._1
          find(element +: elements)
        }
      }

      val freqMap = populateFreq(0, Map())
      freqMap.foreach(freq => queue.enqueue(freq))
      find(Nil).toArray
    }
  }
}
