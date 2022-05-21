package com.algo.arrays

object RotateArray extends App {

  val input = Array(1, 2, 3, 4, 5, 6)

  Solution.rotate(input, 4)
  println(input.mkString(","))

  object Solution {
    def rotate(nums: Array[Int], k: Int): Unit = {
      val effectiveK = k % nums.length

      def swapElements(startingIndex: Int, numOfTimes: Int): Unit = {
        @scala.annotation.tailrec
        def swap(sourceIndex: Int, numSourceIndex: Int, currTimes: Int): Unit = {
          if (currTimes == numOfTimes) {
            ()
          } else {
            val destIndex = (sourceIndex + effectiveK) % nums.length
            val numDestIndex = nums(destIndex)
            nums(destIndex) = numSourceIndex
            swap(destIndex, numDestIndex, currTimes + 1)
          }
        }

        swap(startingIndex, nums(startingIndex), 0)
      }

      @scala.annotation.tailrec
      def gcf(num1: Int, num2: Int): Int = {
        if (num1 == 0 && num2 == 0) {
          0
        } else if (num1 == 0) {
          num2
        } else if (num2 == 0) {
          num1
        } else {
          if (num1 < num2) {
            gcf(num1, num2 % num1)
          } else {
            gcf(num1 % num2, num2)
          }
        }
      }

      def lcm(num1: Int, num2: Int): Int = {
        num1 * num2 / gcf(num1, num2)
      }

      val rotationsInAGo = lcm(effectiveK, nums.length) / effectiveK

      @scala.annotation.tailrec
      def rotateArray(indexToBeRotated: Int, numToBeRotated: Int, numOfElementsRotated: Int): Unit = {
        println(nums.mkString(","))
        if (numOfElementsRotated == nums.length) {
          ()
        } else {
          swapElements(indexToBeRotated, rotationsInAGo)
          rotateArray(indexToBeRotated + 1, nums(indexToBeRotated + 1), numOfElementsRotated + rotationsInAGo)
        }
      }

      if (effectiveK == 0 || effectiveK == nums.length || nums.length == 1) {
        ()
      } else {
        rotateArray(0, nums(0), 0)
      }
    }
  }

}
