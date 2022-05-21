package com.algo.dp

object MaxNonOverlappingSubArraysTargetSum2 extends App {

  val n = Array(5,7,24,-8,3,3,-8,14,1,14,-10,7,15,-7,-5)
  val t = 50

  val res = Solution.maxNonOverlapping(n, t)
  println(res)

  object Solution {
    def maxNonOverlapping(nums: Array[Int], target: Int): Int = {

      val prefixSums = Array.ofDim[Int](nums.length, nums.length)

      def sum(currIndex: Int): Int = {
        @scala.annotation.tailrec
        def doSum(index: Int): Int = {
          if(index < 0) {
            0
          } else if(index == currIndex) {
            if(nums(currIndex) == target) {
              (index to 0 by -1).foreach(i => {
                prefixSums(i)(currIndex) = 0
              })
              1
            } else {
              prefixSums(index)(currIndex) = nums(currIndex)
              doSum(index - 1)
            }
          } else {
            if(nums(currIndex) + prefixSums(index)(currIndex - 1) == target) {
              (currIndex to 0 by -1).foreach(i => {
                prefixSums(i)(currIndex) = 0
              })
              1
            } else {
              prefixSums(index)(currIndex) = nums(currIndex) + prefixSums(index)(currIndex - 1)
              doSum(index - 1)
            }
          }
        }
        doSum(currIndex)
      }

      @scala.annotation.tailrec
      def find(index: Int, curr: Int): Int = {
        if(index == nums.length) {
          curr
        } else {
          val output = sum(index)
          find(index + 1, curr + output)
        }
      }

      find(0, 0)
    }
  }

}
