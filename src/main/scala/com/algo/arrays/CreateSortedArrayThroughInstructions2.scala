package com.algo.arrays

import scala.collection.mutable.ArrayBuffer

object CreateSortedArrayThroughInstructions2 extends App {
  object Solution {
    def createSortedArray(instructions: Array[Int]): Int = {

      val modulo: Int = (Math.pow(10, 9).toInt + 7)

      def findMaxInsertPositionOf(num: Int, array: ArrayBuffer[Int]) = {
        @scala.annotation.tailrec
        def find(low: Int, high: Int, pos: Int): Int = {
          if(high < low) {
            if (pos == -1) low else pos
          } else {
            val mid = (low + high) / 2
            if(array(mid) > num) {
              find(low, mid - 1, pos)
            } else if (array(mid) < num) {
              find(mid + 1, high, pos)
            } else {

              find(mid + 1, high, mid + 1)
            }
          }
        }
        find(0, array.length - 1, -1)
      }

      def findMinInsertPositionOf(num: Int, array: ArrayBuffer[Int]) = {
        @scala.annotation.tailrec
        def find(low: Int, high: Int, pos: Int): Int = {
          if(high < low) {
            if (pos == -1) low else pos
          } else {
            val mid = (low + high) / 2
            if(array(mid) > num) {
              find(low, mid - 1, pos)
            } else if (array(mid) < num) {
              find(mid + 1, high, pos)
            } else {
              find(low, mid - 1, mid)
            }
          }
        }
        find(0, array.length - 1, -1)
      }

      //O(n^2). Use data structure like fenwick tree
      @scala.annotation.tailrec
      def findCost(index: Int, arrayTillNow: ArrayBuffer[Int], costTillNow: Int): Int = {
        if(index == instructions.length) {
          costTillNow
        } else {
          val maxPos = findMaxInsertPositionOf(instructions(index), arrayTillNow)
          val minPos = findMinInsertPositionOf(instructions(index), arrayTillNow.take(maxPos))
          val minPosCost = minPos
          val maxPosCost = arrayTillNow.length - maxPos
          val (pos, cost) = if(minPosCost < maxPosCost) (minPos, minPosCost) else (maxPos, maxPosCost)
          val (before, after) = arrayTillNow.splitAt(pos)
          findCost(index + 1, before ++ (instructions(index) +: after), (costTillNow + cost) % modulo)
        }
      }

      findCost(1, ArrayBuffer(instructions.head), 0)
    }
  }
}
