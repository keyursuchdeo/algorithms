package com.algo.arrays

object MinimumRemovalsMakeMountainArray extends App {

//  val a = Array(2,1,1,5,6,2,3,1)
//  val a = Array(3, 4, -1, 0, 6, 2, 3)
//  val a = Array(4,3,2,1,1,2,3,1)
  val a = Array(1,2,3,4,4,3,2,1)
  val res = Solution.minimumMountainRemovals(a)
  println(res)

  object Solution {
    def minimumMountainRemovals(nums: Array[Int]): Int = {
      val lisForward = Array.fill[Int](nums.length)(1)
      val lisBackward = Array.fill[Int](nums.length)(1)

      @scala.annotation.tailrec
      def findLisForward(startIndex: Int, currIndex: Int): Unit = {
        if(currIndex == nums.length) {
          ()
        } else if (startIndex == currIndex) {
          findLisForward(0, currIndex + 1)
        } else {
          if(nums(currIndex) > nums(startIndex)) {
            lisForward(currIndex) = Math.max(lisForward(currIndex), lisForward(startIndex) + 1)
            findLisForward(startIndex + 1, currIndex)
          } else {
            findLisForward(startIndex + 1, currIndex)
          }
        }
      }

      @scala.annotation.tailrec
      def findLisBackward(startIndex: Int, currIndex: Int): Unit = {
        if(currIndex < 0) {
          ()
        } else if (startIndex == currIndex) {
          findLisBackward(nums.length - 1, currIndex - 1)
        } else {
          if(nums(currIndex) > nums(startIndex)) {
            lisBackward(currIndex) = Math.max(lisBackward(currIndex), lisBackward(startIndex) + 1)
            findLisBackward(startIndex - 1, currIndex)
          } else {
            findLisBackward(startIndex - 1, currIndex)
          }
        }
      }

      @scala.annotation.tailrec
      def findMaxMountainLength(index: Int = 1, currMax: Int = 0): Int = {
        if(index == nums.length - 1) {
          currMax
        } else {
          if(lisBackward(index) == 1 || lisForward(index) == 1) {
            findMaxMountainLength(index + 1, currMax)
          } else {
            findMaxMountainLength(index + 1, Math.max(currMax, (lisBackward(index) + lisForward(index) - 1)))
          }
        }
      }

      if(nums.length == 1) {
        1
      } else {
        findLisForward(0, 1)
        findLisBackward(nums.length - 1, nums.length - 2)
        println(lisForward.mkString(","))
        println(lisBackward.mkString(","))
        findMaxMountainLength()
      }

    }
  }
}
