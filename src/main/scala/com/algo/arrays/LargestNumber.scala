package com.algo.arrays

object LargestNumber extends App {
  object Solution {
    def largestNumber(nums: Array[Int]): String = {
      object NumOrder extends Ordering[Int] {
        override def compare(x: Int, y: Int): Int = {
          (s"$x$y").toInt compare (s"$y$x").toInt
        }
      }

      nums.sorted(NumOrder).mkString("")
    }
  }
}
