package com.algo.arrays

object BinSearchRotatedArray extends App {

  val array = Array(4, 5, 6, 7, 8, 1, 2, 3)
  val element = 7
//  val array = Array.empty[Int]
//  val element = 1
  val a = Solution.search(array, element)
  println(a)

  object Solution {
    def search(nums: Array[Int], target: Int): Int = {

      @scala.annotation.tailrec
      def search(l: Int, r: Int): Int = {
        if(r == l) {
          if(nums(l) == target) {
            l
          } else {
            -1
          }
        } else if (r < l) {
          -1
        } else {
          val mid = (l + r) / 2
          if(nums(mid) == target) {
            mid
          } else if (nums(l) <= nums(mid)) {
            if (target >= nums(l) && target <= nums(mid)) {
              search(l, mid)
            } else {
              search(mid + 1, r)
            }
          } else {
            if (target >= nums(mid) && target <= nums(r)) {
              search(mid, r)
            } else {
              search(l, mid - 1)
            }
          }
        }
      }

      if(nums.length == 0) -1 else search(0, nums.length - 1)
    }
  }
}
