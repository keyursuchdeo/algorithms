package com.algo.arrays

object ValidMountainArray extends App {
  object Solution {
    def validMountainArray(arr: Array[Int]): Boolean = {

      @scala.annotation.tailrec
      def check(index: Int = 1, peakFound: Boolean = false): Boolean = {
        if(index == arr.length) {
          peakFound
        } else {
          if(arr(index) == arr(index - 1)) {
            false
          } else {
            if(peakFound) {
              if(arr(index) > arr(index - 1)) {
                false
              } else {
                check(index + 1, peakFound)
              }
            } else {
              if(arr(index) < arr(index - 1)) {
                if(index == 1) {
                  false
                } else {
                  check(index + 1, peakFound = true)
                }
              } else {
                check(index + 1, peakFound)
              }
            }
          }
        }
      }

      check()
    }
  }
}
