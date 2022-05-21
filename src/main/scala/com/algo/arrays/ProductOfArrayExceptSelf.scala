package com.algo.arrays

object ProductOfArrayExceptSelf extends App {
  object Solution {
    def productExceptSelf(nums: Array[Int]): Array[Int] = {
      val forwardProducts = new Array[Int](nums.length)
      val backwardProducts = new Array[Int](nums.length)
      val productsSansSelf = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def fillProductArrays(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else if (index == 0) {
          forwardProducts(index) = nums(index)
          backwardProducts(nums.length - index - 1) = nums(nums.length - index - 1)
          fillProductArrays(index + 1)
        } else {
          forwardProducts(index) = nums(index) * forwardProducts(index - 1)
          backwardProducts(nums.length - index - 1) = nums(nums.length - index - 1) * backwardProducts(nums.length - index)
          fillProductArrays(index + 1)
        }
      }

      @scala.annotation.tailrec
      def products(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else {
          if(index == 0) {
            productsSansSelf(index) = backwardProducts(index + 1)
          } else if (index == nums.length - 1) {
            productsSansSelf(index) = forwardProducts(index - 1)
          } else {
            productsSansSelf(index) = backwardProducts(index + 1) * forwardProducts(index - 1)
          }
          products(index + 1)
        }
      }

      fillProductArrays(0)
      products(0)
      productsSansSelf
    }
  }
}
