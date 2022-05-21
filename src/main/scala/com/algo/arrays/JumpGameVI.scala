package com.algo.arrays

object JumpGameVI extends App {
  object Solution {
    def maxResult(nums: Array[Int], k: Int): Int = {

      @scala.annotation.tailrec
      def calculate(index: Int, deque: Vector[Int]): Int = {
        if(index == nums.length) {
          nums(nums.length - 1)
        } else {
          if(deque.nonEmpty && deque.head < index - k) {
            calculate(index, deque.tail)
          } else {
            nums(index) = nums(index) + deque.headOption.map(h => nums(h)).getOrElse(0)
            if(deque.nonEmpty && nums(deque.last) <= nums(index)) {
              calculate(index, deque.dropRight(1))
            } else {
              calculate(index + 1, deque :+ index)
            }
          }
        }
      }

      calculate(1, Vector[Int](0))
    }
  }
}
