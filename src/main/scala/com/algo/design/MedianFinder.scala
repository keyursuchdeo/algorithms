package com.algo.design

import scala.collection.mutable

object MainMedianFinder extends App {
  val finder = new MedianFinder()
  println(finder.findMedian()) // -> 1.0
  finder.addNum(1)
  println(finder.findMedian()) // -> 1.0
  finder.addNum(2)
  println(finder.findMedian()) // -> 1.5
  finder.addNum(3)
  println(finder.findMedian()) // -> 2
}

class MedianFinder() {

  object MinOrder extends Ordering[Int] {
    def compare(x:Int, y:Int): Int = y compare x
  }

  /** initialize your data structure here. */
  private var minHeap = new mutable.PriorityQueue[Int]()(MinOrder)
  private var maxHeap = new mutable.PriorityQueue[Int]()

  def addNum(num: Int): Unit = {
    (minHeap.headOption, maxHeap.headOption) match {
      case (None, None) => minHeap.enqueue(num)
      case (_, _) => add(num)
    }
  }

  private def add(num: Int): Unit = {
    if(num <= findMedian()) {
      maxHeap.enqueue(num)
      balanceHeaps()
    } else {
      minHeap.enqueue(num)
      balanceHeaps()
    }
  }

  private def balanceHeaps(): Unit = {
    if(minHeap.size - maxHeap.size >= 2) {
      maxHeap.enqueue(minHeap.dequeue())
    } else if (maxHeap.size - minHeap.size >= 2) {
      minHeap.enqueue(maxHeap.dequeue())
    } else {
      ()
    }
  }

  def findMedian(): Double = {
    if(minHeap.size > maxHeap.size) {
      minHeap.head
    } else if (minHeap.size < maxHeap.size) {
      maxHeap.head
    } else if (minHeap.isEmpty && maxHeap.isEmpty) {
      0
    } else {
      (minHeap.head + maxHeap.head) / 2.0
    }
  }

}
