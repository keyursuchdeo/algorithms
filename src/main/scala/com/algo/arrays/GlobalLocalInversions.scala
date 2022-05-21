package com.algo.arrays

object GlobalLocalInversions extends App {
  object Solution {
    def isIdealPermutation(A: Array[Int]): Boolean = {

      @scala.annotation.tailrec
      def checkIfIdeal(index: Int): Boolean = {
        if(index == A.length) {
          true
        } else {
          if(A(index) == index || A(index) - index > 1) {
            false
          } else {
            checkIfIdeal(index + 1)
          }
        }
      }

      checkIfIdeal(0)
    }
  }
}
