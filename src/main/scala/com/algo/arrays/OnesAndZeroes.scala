package com.algo.arrays

object OnesAndZeroes extends App {

  object Solution {
    def findMaxForm(strs: Array[String], m: Int, n: Int): Int = {
      val countsZeroesOnes = new Array[(Int, Int)](strs.length)
      val remainingZeroesOnes = new Array[(Int, Int)](strs.length)
      val subsetSize = new Array[Int](strs.length)
      var map: Map[(Int, Int, Int), Int] = Map[(Int, Int, Int), Int]()

      def countOnesAndZeroes(string: String): (Int, Int) = {
        val chars = string.toCharArray

        @scala.annotation.tailrec
        def count(index: Int, zeroes: Int, ones: Int): (Int, Int) = {
          if (index == chars.length) {
            (zeroes, ones)
          } else {
            if (chars(index) == '0') {
              count(index + 1, zeroes + 1, ones)
            } else {
              count(index + 1, zeroes, ones + 1)
            }
          }
        }

        count(0, 0, 0)
      }

      @scala.annotation.tailrec
      def fillCounts(index: Int): Unit = {
        if (index == strs.length) {
          ()
        } else {
          countsZeroesOnes(index) = countOnesAndZeroes(strs(index))
          fillCounts(index + 1)
        }
      }

      fillCounts(0)

      def isRemainingMoreThanOrEqualToRequired(remaining: (Int, Int), required: (Int, Int)): Boolean = {
        required._1 >= remaining._1 && required._2 >= remaining._2
      }

      //      @scala.annotation.tailrec
      //      def fillSubsetSize(index: Int): Unit = {
      //        if (index == strs.length) {
      //          ()
      //        } else {
      //          val (zeroes, ones) = countsZeroesOnes(index)
      //          if (zeroes <= m && ones <= n) {
      //            val possibleCountIndices =
      //              (0 until index).collect {
      //                case i if isRemainingMoreThanOrEqualToRequired(remainingZeroesOnes(i), countsZeroesOnes(index)) =>
      //                  (subsetSize(i), i)
      //              }
      //            if(possibleCountIndices.isEmpty) {
      //              subsetSize(index) = 1
      //              remainingZeroesOnes(index) = (m - zeroes, n - ones)
      //            } else {
      //              val (maxSubsetSize, maxSubsetIndex) = possibleCountIndices.maxBy(_._1)
      //              subsetSize(index) = 1 + maxSubsetSize
      //              val (remZeroes, remOnes) = remainingZeroesOnes(maxSubsetIndex)
      //              remainingZeroesOnes(index) = (remZeroes - zeroes, remOnes - ones)
      //            }
      //          } else {
      //            subsetSize(index) = 0
      //            remainingZeroesOnes(index) = (m, n)
      //          }
      //          fillSubsetSize(index + 1)
      //        }
      //      }

      def find(index: Int, remainingZeroes: Int, remainingOnes: Int): Int = {
        if (remainingZeroes < 0 || remainingOnes < 0) {
          Int.MinValue
        } else if (index == strs.length) {
          0
        } else {
          map.get((index, remainingZeroes, remainingOnes)) match {
            case Some(value) => value
            case _ =>
              val (zeroes, ones) = countsZeroesOnes(index)
              val value =
                Math.max(
                  1 + find(index + 1, remainingZeroes - zeroes, remainingOnes - ones),
                  find(index + 1, remainingZeroes, remainingOnes)
                )
              map = map + (((index, remainingZeroes, remainingOnes)) -> value)
              value
          }
        }
      }

//      fillSubsetSize(0)
      subsetSize.max
    }
  }

}
