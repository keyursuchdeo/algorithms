package com.algo.arrays

object FindInMountainArray extends App {

  class MountainArray {
    def get(index: Int): Int = 1

    def length(): Int = 1
  }


  object Solution {
    def findInMountainArray(value: Int, mountainArr: MountainArray): Int = {

      def findPeakLoc(index: Int, valueAtIndex: Int): Int = {
        if(index == 0) {
          1
        } else if (index == mountainArr.length() - 1) {
          -1
        } else {
          val left = mountainArr.get(index - 1)
          val right = mountainArr.get(index + 1)
          if(left < valueAtIndex && right < valueAtIndex) {
            0
          } else if (left < valueAtIndex) {
            1
          } else {
            -1
          }
        }
      }

      @scala.annotation.tailrec
      def findPeak(low: Int, high: Int): Int = {
        if(high < low) {
          -1
        } else {
          val mid = (low + high) / 2
          val peakLoc = findPeakLoc(mid, mountainArr.get(mid))
          if(peakLoc == 0) {
            mid
          } else if (peakLoc == -1) {
            findPeak(low, mid - 1)
          } else {
            findPeak(mid + 1, high)
          }
        }
      }

      @scala.annotation.tailrec
      def findNumInLeft(low: Int, high: Int): Int = {
        if(high < low) {
          -1
        } else {
          val mid = (low + high) / 2
          val valueAtMid = mountainArr.get(mid)
          if(valueAtMid == value) {
            mid
          } else if (valueAtMid < value) {
            findNumInLeft(mid + 1, high)
          } else {
            findNumInLeft(low, mid - 1)
          }
        }
      }

      @scala.annotation.tailrec
      def findNumInRight(low: Int, high: Int): Int = {
        if(high < low) {
          -1
        } else {
          val mid = (low + high) / 2
          val valueAtMid = mountainArr.get(mid)
          if(valueAtMid == value) {
            mid
          } else if (valueAtMid < value) {
            findNumInRight(low, mid - 1)
          } else {
            findNumInRight(mid + 1, high)
          }
        }
      }

      val peak = findPeak(0, mountainArr.length() - 1)
      val numIndex = findNumInLeft(0, peak)
      if(numIndex == -1) findNumInRight(peak + 1, mountainArr.length() - 1) else numIndex
    }
  }

}
