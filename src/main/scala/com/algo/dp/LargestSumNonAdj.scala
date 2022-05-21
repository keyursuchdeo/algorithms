package com.algo.dp

/**
 * This problem was asked by Airbnb.
 *
 * Given a list of integers, write a function that returns the largest sum of non-adjacent numbers. Numbers can be 0 or negative.
 *
 * For example, [2, 4, 6, 2, 5] should return 13, since we pick 2, 6, and 5. [5, 1, 1, 5] should return 10, since we pick 5 and 5.
 *
 * Follow-up: Can you do this in O(N) time and constant space?
 */

object LargestSumNonAdj extends App {

//  val a = Array(5, 1, 1, 5)
//  val a = Array(2, 4, 6, 2, 5)
//  val a = Array(-1, 10, 20, 9)
  val a = Array(-50, -1, -2, 200, -4, -5, -100)
  val res = Solution.largestSum(a)
  println(res)

  object Solution {
    def largestSum(nums: Array[Int]): Int = {
      val sums = new Array[Int](nums.length)
      @scala.annotation.tailrec
      def fill(index: Int): Unit = {
        if(index == nums.length) {
          ()
        } else {
          if (index == 0) {
            sums(index) = nums(index)
            fill(index + 1)
          } else if (index == 1){
            sums(index) = Math.max(sums(index - 1), nums(index))
            fill(index + 1)
          } else {
            sums(index) =
              Math.max(
                Math.max(
                  Math.max(sums(index - 1), (sums(index - 2) + nums(index))),
                  sums(index - 2)
                ),
                nums(index)
              )
            fill(index + 1)
          }
        }
      }

      fill(0)
      println(sums.mkString(","))
      sums(nums.length - 1)
    }
  }
}
