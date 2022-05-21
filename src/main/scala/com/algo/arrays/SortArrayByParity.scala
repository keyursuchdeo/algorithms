package com.algo.arrays

object SortArrayByParity extends App {
  object Solution {
    def sortArrayByParity(A: Array[Int]): Array[Int] = {
      @scala.annotation.tailrec
      def sort(index: Int, startOddIndex: Int): Unit = {
        if(index == A.length) {
          ()
        } else {
          if(A(index) % 2 == 1) {
            if(startOddIndex == -1) {
              sort(index + 1, index)
            } else {
              sort(index + 1, startOddIndex)
            }
          } else {
            if(startOddIndex == -1) {
              sort(index + 1, startOddIndex)
            } else {
              val firstOdd = A(startOddIndex)
              A(startOddIndex) = A(index)
              A(index) = firstOdd
              sort(index + 1, startOddIndex + 1)
            }
          }
        }
      }

      sort(0, -1)
      A
    }
  }
}
