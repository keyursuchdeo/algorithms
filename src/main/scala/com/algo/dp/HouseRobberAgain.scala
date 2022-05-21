package com.algo.dp

object HouseRobberAgain extends App {
  object Solution {
    def rob(nums: Array[Int]): Int = {
      val amount = new Array[Int](nums.length)

      @scala.annotation.tailrec
      def calculate(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else if (index == 0) {
          amount(index) = nums(index)
          calculate(index + 1)
        } else if (index == 1) {
          amount(index) = Math.max(amount(index - 1), nums(index))
          calculate(index + 1)
        } else {
          if(amount(index - 2) == amount(index - 1)) {
            amount(index) = amount(index - 1) + nums(index)
          } else {
            amount(index) = Math.max(amount(index - 1), amount(index - 1) + nums(index))
          }
          calculate(index + 1)
        }
      }

      calculate(0)
      amount(nums.length - 1)
    }
  }
}
