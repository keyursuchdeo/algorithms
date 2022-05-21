package com.algo.arrays

import scala.collection.mutable

object CarPooling extends App {

  val t = new Array[Array[Int]](30)
  val c = 490

  t(0) = Array(94,780,863)
  t(1) = Array(40,573,889)
  t(2) = Array(48,125,620)
  t(3) = Array(73,150,447)
  t(4) = Array(6,116,786)
  t(5) = Array(72,171,548)
  t(6) = Array(35,267,950)
  t(7) = Array(32,169,481)
  t(8) = Array(7,595,829)
  t(9) = Array(65,926,982)
  t(10) = Array(49,770,832)
  t(11) = Array(5,232,851)
  t(12) = Array(30,413,783)
  t(13) = Array(5,424,454)
  t(14) = Array(37,218,777)
  t(15) = Array(22,143,952)
  t(16) = Array(23,402,672)
  t(17) = Array(65,755,847)
  t(18) = Array(20,193,939)
  t(19) = Array(100,586,759)
  t(20) = Array(13,339,954)
  t(21) = Array(40,665,767)
  t(22) = Array(28,209,736)
  t(23) = Array(27,11,761)
  t(24) = Array(5,904,911)
  t(25) = Array(56,54,332)
  t(26) = Array(11,91,424)
  t(27) = Array(8,379,826)
  t(28) = Array(33,516,676)
  t(29) = Array(66,930,960)

  val res = Solution.carPooling(t, c)
  println(res)

  object Solution {
    def carPooling(trips: Array[Array[Int]], capacity: Int): Boolean = {
      object TripOrder extends Ordering[Array[Int]] {
        override def compare(x: Array[Int], y: Array[Int]): Int = {
          val Array(_, s1, e1) = x
          val Array(_, s2, e2) = y

          if (s1 == s2) {
            e2 compare e1
          } else {
            s2 compare s1
          }
        }
      }


      val queue: mutable.PriorityQueue[Array[Int]] = scala.collection.mutable.PriorityQueue.empty(TripOrder)

      @scala.annotation.tailrec
      def separateTripsByTimes(prev: Array[Int]): Boolean = {
        if (queue.isEmpty) {
          true
        } else {
          val Array(pNum, pStart, pEnd) = prev
          val curr = queue.dequeue()
          val Array(cNum, cStart, cEnd) = curr

          if (pEnd <= cStart) {
            separateTripsByTimes(Array(cNum, cStart, cEnd))
          } else {
            val combinedCapacity = pNum + cNum
            if (combinedCapacity > capacity) {
              false
            } else {
              if(pEnd < cEnd) {
                val overlappingCurr = Array(combinedCapacity, cStart, pEnd)
                val remainingTrip = Array(cNum, pEnd, cEnd)
                queue.enqueue(remainingTrip)
                separateTripsByTimes(overlappingCurr)
              } else if (pEnd > cEnd) {
                val overlappingCurr = Array(combinedCapacity, cStart, cEnd)
                val remainingTrip = Array(cNum, cEnd, pEnd)
                queue.enqueue(remainingTrip)
                separateTripsByTimes(overlappingCurr)
              } else {
                val overlappingCurr = Array(combinedCapacity, pStart, cEnd)
                separateTripsByTimes(overlappingCurr)
              }
            }
          }
        }
      }

      if(trips.isEmpty) {
        true
      } else {
        queue.enqueue(trips: _*)
        val a = queue.dequeue()
        println(a.mkString(","))
        separateTripsByTimes(a)
      }
    }
  }

}
