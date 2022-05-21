package com.algo.arrays

object ProductOfArrayExceptSelf2 extends App {
  object Solution {
    def productExceptSelf(nums: Array[Int]): Array[Int] = {
      val productsSansSelf = new Array[Int](nums.length)
      @scala.annotation.tailrec
      def initProductsSansSelf(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else if (index == 0) {
          productsSansSelf(index) = nums(index)
          initProductsSansSelf(index + 1)
        } else {
          productsSansSelf(index) = nums(index) * productsSansSelf(index - 1)
          initProductsSansSelf(index + 1)
        }
      }

      @scala.annotation.tailrec
      def fillProductsSansSelf(index: Int, currBackwardProduct: Int): Unit = {
        if (index == nums.length - 1) {
          productsSansSelf(index) = nums(index)
          fillProductsSansSelf(index - 1, nums(index))
        } else if (index == 0) {
          productsSansSelf(index) = currBackwardProduct
          ()
        } else {
          productsSansSelf(index) = productsSansSelf(index - 1) * currBackwardProduct
          fillProductsSansSelf(index - 1, currBackwardProduct * nums(index))
        }
      }

      initProductsSansSelf(0)
      fillProductsSansSelf(nums.length - 1, 1)
      productsSansSelf
    }
  }
}
