package com.algo.arrays

object PartitionLabels extends App {

  val str = "qiejxqfnqceocmy"
  val res = Solution.partitionLabels(str)
  println(res)

  object Solution {
    def partitionLabels(S: String): List[Int] = {
      val chars = S.toCharArray
      val lastCharPositions = Array.fill[Int](26)(-1)


      @scala.annotation.tailrec
      def fillLastCharPositions(index: Int): Unit = {
        if (index == chars.length) {
          ()
        } else {
          lastCharPositions(chars(index) - 'a') = index
          fillLastCharPositions(index + 1)
        }
      }

      def allCharsBetweenIndices(from: Int, to: Int): Set[Char] = {
        @scala.annotation.tailrec
        def allCharsBetween(index: Int, charsBetween: Set[Char]): Set[Char] = {
          if (index > to || index == chars.length) {
            charsBetween
          } else {
            allCharsBetween(index + 1, charsBetween + chars(index))
          }
        }

        allCharsBetween(from, Set())
      }

      def calculateMaxLastCharPosition(index: Int): Int = {
        @scala.annotation.tailrec
        def calculate(min: Int, max: Int): Int = {
          val allCharsBetween: Set[Char] = allCharsBetweenIndices(min, max)
          val currMaxLastCharPosition =
            allCharsBetween.map(char => {
              lastCharPositions(char - 'a')
            }).max
          if(currMaxLastCharPosition == max) {
            currMaxLastCharPosition
          } else {
            calculate(max, currMaxLastCharPosition)
          }
        }
        calculate(index, lastCharPositions(chars(index) - 'a'))
      }

      @scala.annotation.tailrec
      def partition(index: Int, partitions: Seq[Int]): List[Int] = {
        if (index == chars.length) {
          partitions.reverse.toList
        } else {
          if (lastCharPositions(chars(index) - 'a') == index) {
            partition(index + 1, 1 +: partitions)
          } else {
            val maxLastCharPosition = calculateMaxLastCharPosition(index)
            partition(maxLastCharPosition + 1, (maxLastCharPosition - index + 1) +: partitions)
          }
        }
      }

      fillLastCharPositions(0)
      println(lastCharPositions.mkString(","))
      partition(0, Nil)

    }
  }

}
