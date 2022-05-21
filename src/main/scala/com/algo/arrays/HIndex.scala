package com.algo.arrays

object HIndex extends App {
  object Solution {
    def hIndex(citations: Array[Int]): Int = {
      val numOfCitations = citations.length
      val buckets = new Array[Int](citations.length + 1)

      @scala.annotation.tailrec
      def fillBuckets(index: Int): Unit = {
        if(index == numOfCitations) {
          ()
        } else {
          if(citations(index) >= numOfCitations) {
            buckets(numOfCitations) = buckets(numOfCitations) + 1
          } else {
            buckets(index) = buckets(index) + 1
          }
          fillBuckets(index + 1)
        }
      }

      @scala.annotation.tailrec
      def calculate(index: Int, currCount: Int): Int = {
        if(currCount >= index) {
          currCount
        } else {
          calculate(index - 1, currCount + buckets(index))
        }
      }

      fillBuckets(0)
      calculate(numOfCitations, 0)

    }
  }
}
