package com.algo.arrays

object FindDuplicateNumber extends App {

//  val a = Array(1,3,4,2,2)
  val a = Array(3,1,3,4,2)
//  val a = Array(2, 5, 9, 6, 9, 3, 8, 9, 7, 1)
  val res = Solution.findDuplicate(a)
  println(res)

  object Solution {
    def findDuplicate(nums: Array[Int]): Int = {
      var tortoise = nums(0)
      var hare = nums(0)
      do {
        tortoise = nums(tortoise)
        hare = nums(nums(hare))
      } while (tortoise != hare)

      tortoise = nums(0)
      while(tortoise != hare) {
        tortoise = nums(tortoise)
        hare = nums(hare)
      }

      hare
    }
  }
}
