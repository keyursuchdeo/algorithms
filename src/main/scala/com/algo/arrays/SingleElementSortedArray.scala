package com.algo.arrays

object SingleElementSortedArray extends App {
  val a = Array(1)
  val res = Solution.singleNonDuplicate(a)
  println(res)
  object Solution {
    def singleNonDuplicate(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def find(l: Int, h: Int): Int = {
        if (h < l) {
          -1
        } else {
          val mid = (l + h) / 2
          if (mid - 1 >=0 && nums(mid) == nums(mid - 1)){
            if((mid + 1) % 2 == 0) {
              find(mid + 1, h)
            } else {
              find(l, mid - 2)
            }
          } else if (mid + 1 < nums.length && nums(mid) == nums(mid + 1)) {
            if((nums.length - mid) % 2 == 0 ) {
              find(l, mid - 1)
            } else {
              find(mid + 2, h)
            }
          } else {
            nums(mid)
          }
        }
      }

      find(0, nums.length - 1)
    }
  }
}
