package com.algo.arrays

object SquaresSortedArray extends App {

  object Solution {
    def sortedSquares(nums: Array[Int]): Array[Int] = {
      @scala.annotation.tailrec
      def findIndexOfHighestNegative(low: Int, high: Int, index: Int): Int = {
        if (high < low) {
          index
        } else {
          val mid = (low + high) / 2
          if (nums(mid) >= 0) {
            findIndexOfHighestNegative(low, mid - 1, index)
          } else {
            findIndexOfHighestNegative(mid + 1, high, mid)
          }
        }
      }

      @scala.annotation.tailrec
      def calculateSquares(negIndex: Int, posIndex: Int, squares: Seq[Int]): Array[Int] = {
        if (posIndex == nums.length && negIndex < 0) {
          squares.reverse.toArray
        } else if (posIndex == nums.length) {
          (squares.reverse ++ (negIndex to 0 by -1).map(index => {
            nums(index) * nums(index)
          })).toArray
        } else if (negIndex < 0) {
          (squares.reverse ++ (posIndex until nums.length).map(index => {
            nums(index) * nums(index)
          })).toArray
        } else {
          if (nums(posIndex) < (-1 * nums(negIndex))) {
            calculateSquares(negIndex, posIndex + 1, (nums(posIndex) * nums(posIndex)) +: squares)
          } else {
            calculateSquares(negIndex - 1, posIndex, (nums(negIndex) * nums(negIndex)) +: squares)
          }
        }
      }

      val indexOfHighestNegative = findIndexOfHighestNegative(0, nums.length - 1, -1)
      calculateSquares(indexOfHighestNegative, indexOfHighestNegative + 1, Nil)
    }
  }

}
