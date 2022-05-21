package com.algo.arrays

object BinSearch extends App {

  val array = Array(1, 3, 5, 6)
  val element = 7
  val a = Solution.searchInsert(array, element)
  println(a)

  object Solution {
    def searchInsert(nums: Array[Int], target: Int): Int = {

      @scala.annotation.tailrec
      def search(l: Int, r: Int): Int = {
        if(r == l) {
          if(nums(l) == target) {
            l
          } else if (nums(l) > target){
            l
          } else {
            l + 1
          }
        } else if (r < l) {
          l
        } else {
          val mid = (l + r) / 2
          if (target < nums(mid)) {
            search(l, mid - 1)
          } else if (target > nums(mid)){
            search(mid + 1, r)
          } else {
            mid
          }
        }
      }

      if(nums.length == 0) 0 else search(0, nums.length - 1)
    }
  }
}
