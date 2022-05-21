package com.algo.ufind

import scala.collection.mutable

object LongestConsecutiveSequence2 extends App {
//  val a = Array(100, 4, 200, 1, 3, 2)
//  val a = Array(1, 2, 3, 4, 5, 6)
val a = Array(6, 5, 4, 3, 2, 1)
  val res = Solution.longestConsecutive(a)
  println(res)

  object Solution {
    def longestConsecutive(nums: Array[Int]): Int = {
      var set: mutable.Set[Int] = mutable.Set(nums: _*)
      @scala.annotation.tailrec
      def find(index: Int, maxLen: Int): Int = {
        if(index == nums.length || set.isEmpty) {
          maxLen
        } else {
          set = set - nums(index)
          val fLen = findForwardConsecutiveNums(nums(index) + 1, 1)
          val bLen = findBackwardConsecutiveNums(nums(index) - 1, 0)
          val len = fLen + bLen
          if(len > maxLen) {
            find(index + 1, len)
          } else {
            find(index + 1, maxLen)
          }
        }
      }

      @scala.annotation.tailrec
      def findForwardConsecutiveNums(num: Int, length: Int): Int = {
        if (set.contains(num)) {
          set = set - (num)
          findForwardConsecutiveNums(num + 1, length + 1)
        } else {
          length
        }
      }

      @scala.annotation.tailrec
      def findBackwardConsecutiveNums(num: Int, length: Int): Int = {
        if (set.contains(num)) {
          set = set - (num)
          findBackwardConsecutiveNums(num - 1, length + 1)
        } else {
          length
        }
      }

      find(0, 0)
    }
  }
}
