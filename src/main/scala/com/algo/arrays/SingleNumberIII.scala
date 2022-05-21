package com.algo.arrays

object SingleNumberIII extends App {
  object Solution {
    def singleNumber(nums: Array[Int]): Array[Int] = {
      lazy val xorAll: Int = nums.fold(0)(_ ^ _)
      lazy val numFirstEnabledBit = xorAll & (xorAll - 1) ^ xorAll
      lazy val (p1, p2) = nums.partition(num => (num & numFirstEnabledBit) == numFirstEnabledBit)
      Array(p1.fold(0)(_ ^ _), p2.fold(0)(_ ^ _))
    }
  }
}
