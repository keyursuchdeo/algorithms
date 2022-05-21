package com.algo.arrays
import scala.collection.immutable

object ReduceArraySizeToTheHalf extends App {
  object Solution {
    def minSetSize(arr: Array[Int]): Int = {

      val halfSize = Math.ceil(arr.length / 2.0).toInt

      @scala.annotation.tailrec
      def recordFrequency(index: Int, freq: Map[Int, Int]): Map[Int, Int] = {
        if (index == arr.length) {
          freq
        } else {
          recordFrequency(index + 1, freq + (arr(index) -> (freq.getOrElse(arr(index), 0) + 1)))
        }
      }

      object MaxOrder extends Ordering[(Int, Int)] {
        override def compare(x: (Int, Int), y: (Int, Int)): Int = {
          x._2 compare y._2
        }
      }
      val maxHeap = scala.collection.mutable.PriorityQueue.empty(MaxOrder)

      @scala.annotation.tailrec
      def findMinSetSize(removedSize: Int, elementsRemoved: Int): Int = {
        if(removedSize >= halfSize) {
          elementsRemoved
        } else {
          val (_, freq) = maxHeap.dequeue()
          findMinSetSize(removedSize + freq, elementsRemoved + 1)
        }
      }

      val freqMap: immutable.Seq[(Int, Int)] = recordFrequency(0, Map()).toList
      freqMap.foreach(freq => maxHeap.enqueue(freq))
      findMinSetSize(0, 0)
    }
  }
}
