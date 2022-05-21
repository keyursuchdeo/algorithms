package com.algo.dp

object HouseRobberIICircular extends App {
  object Solution {
    def rob(nums: Array[Int]): Int = {

      @scala.annotation.tailrec
      def calculate(index: Int, input: Array[Int], amount: Array[Int]): Array[Int] = {
        if(index == input.length) {
          amount
        } else if (index == 0) {
          amount(index) = input(index)
          calculate(index + 1, input, amount)
        } else if (index == 1) {
          amount(index) = Math.max(amount(index - 1), input(index))
          calculate(index + 1, input, amount)
        } else {
          if(amount(index - 2) == amount(index - 1)) {
            amount(index) = amount(index - 1) + input(index)
          } else {
            amount(index) = Math.max(amount(index - 1), amount(index - 2) + input(index))
          }
          calculate(index + 1, input, amount)
        }
      }

      if(nums.length == 0) 0 else if(nums.length == 1) nums.head else {
        val amount1 = calculate(0, nums.take(nums.length - 1), new Array[Int](nums.length - 1))
        val amount2 = calculate(0, nums.tail, new Array[Int](nums.length - 1))
        Math.max(amount1(nums.length - 2), amount2(nums.length - 2))
      }
    }
  }
}
