package com.algo.arrays

object BinSearchRotatedArrayDups extends App {

  val array = Array(3, 1)
  val element = 2
//  val array = Array.empty[Int]
//  val element = 1
  val a = Solution.search(array, element)
  println(a)

  object Solution {
    def search(nums: Array[Int], target: Int): Boolean = {

      def search(l: Int, r: Int): Boolean = {
        if(r == l) {
          if(nums(l) == target) {
            true
          } else {
            false
          }
        } else if (r < l) {
          false
        } else {
          val mid = (l + r) / 2
          if(nums(mid) == target) {
            true
          } else if (nums(l) < nums(mid)) {
            if (target >= nums(l) && target <= nums(mid)) {
              search(l, mid)
            } else {
              search(mid + 1, r)
            }
          } else if (nums(mid) > nums(l)) {
            if (target >= nums(mid) && target <= nums(r)) {
              search(mid, r)
            } else {
              search(l, mid - 1)
            }
          } else if (nums(mid) == nums(l) && nums(mid) != nums(r)) {
            search(mid + 1, r)
          } else {
            search(l, mid - 1) || search(mid + 1, r)
          }
        }
      }

      if(nums.length == 0) false else search(0, nums.length - 1)
    }
  }
}
