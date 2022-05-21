package com.algo.arrays

object LongestTurbulentSubarray extends App {

  val a = Array(9,4,2,10,7,8,8,1,9)
  val res = Solution.maxTurbulenceSize(a)
  println(res)

  object Solution {
    def maxTurbulenceSize(arr: Array[Int]): Int = {

      def isGreaterThanNext(curr: Int, next: Int): Boolean = {
        arr(curr) - arr(next) > 0
      }

      def isLessThanNext(curr: Int, next: Int): Boolean = {
        arr(curr) - arr(next) < 0
      }


      @scala.annotation.tailrec
      def find(index: Int, maxSize: Int, currSize: Int, currOp: (Int, Int) => Boolean, nextOp: (Int, Int) => Boolean): Int = {
        if(index + 1 == arr.length) {
          Math.max(maxSize, currSize)
        } else {
          if(currOp(index, index + 1)) {
            find(index + 1, maxSize, currSize + 1, nextOp, currOp)
          } else {
            if(arr(index) == arr(index + 1)) {
              find(index + 1, Math.max(maxSize, currSize + 1), currSize = 0, currOp, nextOp)
            } else {
              find(index, Math.max(maxSize, currSize + 1), currSize = 0, nextOp, currOp)
            }
          }
        }
      }

      find(0, 0, 0, isGreaterThanNext, isLessThanNext)
    }
  }
}
