package com.algo.arrays

object CheckArrayFormationThroughConcatenation extends App {
  object Solution {
    def canFormArray(arr: Array[Int], pieces: Array[Array[Int]]): Boolean = {

      def notePositionOfArrElements(): Map[Int, Int] = {
        arr.zipWithIndex.toMap
      }

      def isIncreasing(array: Array[Int]): Boolean = {
        @scala.annotation.tailrec
        def check(index: Int): Boolean = {
          if(index == array.length) {
            true
          } else {
            if(array(index) == -1) {
              false
            } else {
              if(index == 0 || array(index) - array(index - 1) == -1) {
                check(index + 1)
              } else {
                false
              }
            }
          }
        }
        check(0)
      }

      @scala.annotation.tailrec
      def checkIfArrayCanBeFormed(index: Int, positions: Map[Int, Int]): Boolean = {
        if(index == pieces.length) {
          true
        } else {
          val piece = pieces(index)
          if (isIncreasing(piece.map(positions.getOrElse(_, -1)))) {
            checkIfArrayCanBeFormed(index + 1, positions)
          } else {
            false
          }
        }
      }

      checkIfArrayCanBeFormed(0, notePositionOfArrElements())
    }
  }
}
