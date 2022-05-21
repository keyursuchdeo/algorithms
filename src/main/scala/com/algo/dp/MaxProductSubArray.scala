package com.algo.dp

object MaxProductSubArray extends App {

  val n = Array(-2, 3, 2, -4)
  val res = Solution.maxProduct(n)
  println(res)

  object Solution {
    def maxProduct(nums: Array[Int]): Int = {
      val products = Array.ofDim[Int](nums.length, nums.length)

      def fill(index: Int): Unit = {
        (index to 0 by -1).foreach(currIndex => {
          if(currIndex == index) {
            products(currIndex)(index) = nums(index)
          } else {
            products(currIndex)(index) = nums(index) * products(currIndex)(index - 1)
          }
        })
      }

      @scala.annotation.tailrec
      def fillProducts(index: Int): Unit = {
        if(index == products.length) {
          ()
        } else {
          fill(index)
          fillProducts(index + 1)
        }
      }

      fillProducts(0)
      println(products.map(_.mkString(",")).mkString("|"))
      products.map(_.max).max
    }
  }
}
