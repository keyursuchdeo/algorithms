package com.algo.heap

object FindKPairsWithSmallestSums extends App {
  object Solution {
    def kSmallestPairs(nums1: Array[Int], nums2: Array[Int], k: Int): List[List[Int]] = {
      object MinOrder extends Ordering[List[Int]] {
        override def compare(x: List[Int], y: List[Int]): Int = {
          (y.head + y.tail.head) compare (x.head + x.tail.head)
        }
      }
      val minHeap = scala.collection.mutable.PriorityQueue.empty(MinOrder)

      @scala.annotation.tailrec
      def fillHeap(index1: Int, index2: Int): Unit = {
        if(index1 == nums1.length) {
          ()
        } else if (index2 == nums2.length) {
          fillHeap(index1 + 1, 0)
        } else {
          minHeap.enqueue(List(nums1(index1), nums2(index2)))
          fillHeap(index1, index2 + 1)
        }
      }

      fillHeap(0, 0)
      (1 to Math.min(k, nums1.length * nums2.length)).map(_ => minHeap.dequeue()).toList
    }
  }
}
