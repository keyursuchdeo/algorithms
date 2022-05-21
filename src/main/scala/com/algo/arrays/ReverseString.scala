package com.algo.arrays

object ReverseString extends App {
  object Solution {
    def reverseString(s: Array[Char]): Unit = {
      @scala.annotation.tailrec
      def reverse(low: Int, high: Int): Unit = {
        if (high <= low) {
          ()
        } else {
          swap(low, high)
          reverse(low + 1, high - 1)
        }
      }

      def swap(index1: Int, index2: Int): Unit = {
        val temp = s(index1)
        s(index1) = s(index2)
        s(index2) = temp
      }

      reverse(0, s.length - 1)
    }
  }
}
