package com.algo.arrays

object LongestMountainInArray extends App {

  object Solution {
    def longestMountain(A: Array[Int]): Int = {
      @scala.annotation.tailrec
      def findLongestMountain(index: Int = 1, currUpwardLen: Int, currDownwardLen: Int, maxLen: Int): Int = {
        if (index >= A.length) {
          if(currDownwardLen > 0) {
            Math.max(currUpwardLen + currDownwardLen + 1, maxLen)
          } else {
            maxLen
          }
        } else {
          if (A(index) - A(index - 1) > 0) {
            if(currDownwardLen > 0) {
              findLongestMountain(index + 1, currUpwardLen = 1, currDownwardLen = 0, Math.max(currUpwardLen + currDownwardLen + 1, maxLen))
            } else {
              findLongestMountain(index + 1, currUpwardLen + 1, currDownwardLen, maxLen)
            }
          } else if (A(index) - A(index - 1) < 0) {
            if(currUpwardLen > 0) {
              findLongestMountain(index + 1, currUpwardLen, currDownwardLen + 1, maxLen)
            } else {
              findLongestMountain(index + 1, currUpwardLen, currDownwardLen, maxLen)
            }
          } else {
            if(currUpwardLen > 0 && currDownwardLen > 0) {
              findLongestMountain(index + 1, currUpwardLen = 0, currDownwardLen = 0, Math.max(currUpwardLen + currDownwardLen + 1, maxLen))
            } else {
              findLongestMountain(index + 1, currUpwardLen = 0, currDownwardLen = 0, maxLen)
            }
          }
        }
      }

      findLongestMountain(index = 1, currUpwardLen = 0, currDownwardLen = 0, maxLen = 0)
    }
  }

}
