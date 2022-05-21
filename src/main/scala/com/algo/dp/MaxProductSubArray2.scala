package com.algo.dp

object MaxProductSubArray2 extends App {

  val n = Array(2, 3, -2, 4)
  val res = Solution.maxProduct(n)
  println(res)

  object Solution {
    def maxProduct(nums: Array[Int]): Int = {
      val maxProducts = new Array[Int](nums.length)
      val minProducts = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def fill(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else if(index == 0) {
          maxProducts(index) = nums(index)
          minProducts(index) = nums(index)
          fill(index + 1)
        } else if (nums(index) > 0){
          maxProducts(index) = Math.max(
            maxProducts(index - 1) * nums(index),
            nums(index)
          )
          minProducts(index) = minProducts(index - 1) * nums(index)
          fill(index + 1)
        } else {
          maxProducts(index) = minProducts(index - 1) * nums(index)
          minProducts(index) = Math.min(
            maxProducts(index - 1) * nums(index),
            nums(index)
          )
          fill(index + 1)
        }
      }

      fill(0)
      println(maxProducts.mkString(","))
      println(minProducts.mkString(","))
      maxProducts.max
    }
  }

}
