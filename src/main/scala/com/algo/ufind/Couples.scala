package com.algo.ufind

import scala.collection.mutable

object Couples extends App {

  val input = Array(3, 2, 0, 1)
  Couples.Solution.minSwapsCouples(input)

  object Solution {
    def minSwapsCouples(row: Array[Int]): Int = {
      val rowWithIndex: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int] ++ row.zipWithIndex.toMap

      @scala.annotation.tailrec
      def swapAndCount(index: Int, numOfSwaps: Int): Int = {
        if(index == row.length) {
          numOfSwaps
        } else {
          if(isNextElemPartOfCouple(index)) {
            swapAndCount(index + 2, numOfSwaps)
          } else {
            if(row(index) % 2 == 0) {
              val toIndex = rowWithIndex(row(index) + 1)
              swap(index + 1, toIndex)
              updateIndexInMap(index + 1, toIndex, row(toIndex), row(index + 1))
              swapAndCount(index + 2, numOfSwaps + 1)
            } else {
              val toIndex = rowWithIndex(row(index) - 1)
              swap(index + 1, toIndex)
              updateIndexInMap(index + 1, toIndex, row(toIndex), row(index + 1))
              swapAndCount(index + 2, numOfSwaps + 1)
            }

          }
        }
      }

      def isNextElemPartOfCouple(index: Int): Boolean = {
        val curr = row(index)
        if (curr % 2 == 0) row(index + 1) == curr + 1 else row(index + 1) == curr - 1
      }

      def swap(fromIndex: Int, toIndex: Int): Unit = {
        val temp = row(fromIndex)
        row(fromIndex) = row(toIndex)
        row(toIndex) = temp
      }

      def updateIndexInMap(fromIndex: Int, toIndex: Int, fromNum: Int, toNum: Int) = {
        rowWithIndex += ((toNum, fromIndex))
        rowWithIndex += ((fromNum, toIndex))
      }

      val a = swapAndCount(0, 0)
      println(a)
      a
    }
  }
}
