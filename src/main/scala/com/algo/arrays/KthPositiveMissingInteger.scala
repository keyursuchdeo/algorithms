package com.algo.arrays

object KthPositiveMissingInteger extends App {
  object Solution {
    def findKthPositive(arr: Array[Int], k: Int): Int = {
      @scala.annotation.tailrec
      def find(index: Int, currK: Int): Int = {
        if(currK <= 0) {
          arr(index - 1) + (currK - 1)
        } else {
          if(index == arr.length) {
            arr.length
          } else {
            if(index == 0) {
              find(index + 1, currK - (arr(index) - 1))
            } else {
              find(index + 1, currK - (arr(index) - arr(index - 1) - 1))
            }
          }
        }
      }

      find(0, k)
    }
  }
}
