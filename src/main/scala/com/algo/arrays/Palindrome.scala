package com.algo.arrays

object Palindrome extends App {
  object Solution {
    def isPalindrome(x: Int): Boolean = {
      if(x < 0) {
        false
      } else if (x < 10) {
        true
      } else {
        x == calculateReversedNum(x, 0)
      }
    }

    @scala.annotation.tailrec
    def calculateReversedNum(num: Int, reversedNum: Int): Int = {
      if (num == 0) {
        reversedNum
      } else {
        calculateReversedNum(num / 10, (reversedNum * 10) + (num % 10))
      }
    }
  }
}
