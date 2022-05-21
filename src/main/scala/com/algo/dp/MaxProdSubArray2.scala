package com.algo.dp

object MaxProdSubArray2 extends App {
  val a = Array(2, -5, -2, -4, 3)
  val res = Solution.maxProduct(a)
  println(res)

  object Solution {
    def maxProduct(nums: Array[Int]): Int = {

      def max(): Int = {
        var currMax = nums(0)
        var prevMax = nums(0)
        var prevMin = nums(0)
        var currMin = nums(0)
        var ans = nums(0)
        for(i <- 1 until nums.length) {
          currMax = Math.max(Math.max(prevMax * nums(i), prevMin * nums(i)), nums(i))
          currMin = Math.min(Math.min(prevMax * nums(i), prevMin * nums(i)), nums(i))
          ans = Math.max(ans, currMax)
          prevMax = currMax
          prevMin = currMin
        }
        ans
      }

      max()

    }
  }

}
