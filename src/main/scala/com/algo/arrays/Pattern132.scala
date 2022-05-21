package com.algo.arrays

object Pattern132 extends App {

  val input = Array(3, 1, 4, 2)
  val res = Solution.find132pattern(input)
  println(res)

  object Solution {
    def find132pattern(nums: Array[Int]): Boolean = {
      @scala.annotation.tailrec
      def find(i: Int = 0, j: Int = 1, k: Int = 2): Boolean = {
        if(i == j || j == k) {
          false
        } else {
          //nums(j) > nums(k) && nums(k) > nums(i)
          if(nums(j) <= nums(i)) {
            if(j == nums.length - 2) {
              false
            } else {
              find(i, k, k + 1)
            }
          } else if (nums(j) <= nums(k)) {
            if(k == nums.length - 1) {
              false
            } else {
              find(i, j, k + 1)
            }
          } else if (nums(k) <= nums(i)) {
            if(k == nums.length - 2) {
              find(i + 1, j, k)
            } else {
              find(i, k + 1, k + 2)
            }
          } else {
            true
          }
        }
      }

      if(nums.length < 3) false else find()
    }
  }
}
