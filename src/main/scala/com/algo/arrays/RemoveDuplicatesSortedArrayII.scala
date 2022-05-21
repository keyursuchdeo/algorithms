package com.algo.arrays

object RemoveDuplicatesSortedArrayII extends App {
  object Solution {
    def removeDuplicates(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def remove(index: Int, nextSlot: Int, elemCount: Int): Int = {
        if(index == nums.length) {
          nextSlot
        } else {
          if(index == 0) {
            remove(index + 1, nextSlot + 1, elemCount + 1)
          } else if (index == 1) {
            if(nums(index) == nums(index - 1)) {
              remove(index + 1, nextSlot + 1, elemCount + 1)
            } else {
              remove(index + 1, nextSlot + 1, 1)
            }
          } else {
            if(nums(index) == nums(index - 1) && elemCount + 1 > 2) {
              remove(index + 1, nextSlot, elemCount + 1)
            } else if (nums(index) == nums(index - 1)) {
              remove(index + 1, nextSlot + 1, elemCount + 1)
            } else {
              if(nextSlot < index) {
                nums(nextSlot) = nums(index)
              }
              remove(index + 1, nextSlot + 1, 1)
            }
          }
        }
      }

      remove(0, 0, 0)
    }
  }
}
