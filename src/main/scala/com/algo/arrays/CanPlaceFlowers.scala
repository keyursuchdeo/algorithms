package com.algo.arrays

object CanPlaceFlowers extends App {

  val f = Array(0)
  val n = 1
  val res = Solution.canPlaceFlowers(f, n)
  println(res)


  object Solution {
    def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {
      @scala.annotation.tailrec
      def check(index: Int, count: Int): Boolean = {
        if(count == n) {
          true
        } else if (index == flowerbed.length) {
          false
        } else if (flowerbed(index) == 1) {
          check(index + 1, count)
        } else {
          if(index == 0) {
            if(index + 1 >= flowerbed.length || flowerbed(index + 1) == 0) {
              flowerbed(index) = 1
              check(index + 1, count + 1)
            } else {
              check(index + 1, count)
            }
          } else if (index == flowerbed.length - 1) {
            if(index - 1 >= 0 && flowerbed(index - 1) == 0) {
              flowerbed(index) = 1
              check(index + 1, count + 1)
            } else {
              check(index + 1, count)
            }
          } else {
            if(flowerbed(index - 1) == 0 && flowerbed(index + 1) == 0) {
              flowerbed(index) = 1
              check(index + 1, count + 1)
            } else {
              check(index + 1, count)
            }
          }
        }
      }
      if (n == 0) true else if(flowerbed.length < n) false else check(0, 0)
    }
  }
}
