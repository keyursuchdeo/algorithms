package com.algo.arrays

object NonDecreasingArray extends App {

  val a = Array(2,3,3,2,2)
  val res = Solution.checkPossibility(a)
  println(res)

  object Solution {
    def checkPossibility(nums: Array[Int]): Boolean = {

      def calcPrevValue(index: Int): Int = {
        if(index - 2 < 0) {
          nums(index)
        } else {
          if(nums(index - 2) <= nums(index)) {
            nums(index)
          } else {
            nums(index - 1)
          }
        }
      }

      @scala.annotation.tailrec
      def check(index: Int = 1, prev: Int, changeFound: Boolean = false): Boolean = {
        if(index == nums.length) {
          true
        } else {
          if(prev > nums(index)) {
            if(changeFound) {
              false
            } else {
              check(index + 1, calcPrevValue(index), changeFound = true)
            }
          } else {
            check(index + 1, nums(index), changeFound)
          }

        }
      }
      check(prev = nums(0))
    }
  }
}
