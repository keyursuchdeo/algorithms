package com.algo.deque

object  SlidingWindowMax extends App {

  val array = Array(1,3,-1,-3,5,3,6,7)
  val k = 1

//  val array = Array(1, 2, 3, 1, 4, 5, 2, 3, 6)
//  val k = 3

  val result = Solution.maxSlidingWindow(array, k)
  println(result.mkString(","))

  object Solution {
    def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
      @scala.annotation.tailrec
      def find(index: Int, deque: Vector[Int], dequeSize: Int, output: Seq[Int]): Seq[Int] = {
        if (index == nums.length) {
          output
        } else {
          val (dequeAfterRemoval, dequeSizeAfterRemoval) = emptyDequeUntilLess(deque, dequeSize, nums(index))
          val (dequeAfterAddition, dequeSizeAfterAdd) = (dequeAfterRemoval :+ index, dequeSizeAfterRemoval + 1)
          if(index >= k - 1) {
            val max = nums(dequeAfterAddition(0))
            if (dequeAfterAddition(0) == index + 1 - k) {
              find(index + 1, dequeAfterAddition.drop(1), dequeSizeAfterAdd - 1, max +: output)
            } else {
              find(index + 1, dequeAfterAddition, dequeSizeAfterAdd, max +: output)
            }
          } else {
            find(index + 1, dequeAfterAddition, dequeSizeAfterAdd, output)
          }
        }
      }

      def emptyDequeUntilLess(deque: Vector[Int], currDequeSize: Int, num: Int): (Vector[Int], Int) = {

        @scala.annotation.tailrec
        def empty(index: Int, updatedDeque: Vector[Int], updatedDequeSize: Int): (Vector[Int], Int) = {
          if (index < 0 || nums(deque(index)) >= num) {
            (updatedDeque, updatedDequeSize)
          } else {
            empty(index - 1, updatedDeque.dropRight(1), updatedDequeSize - 1)
          }
        }

        empty(currDequeSize - 1, deque, currDequeSize)
      }

      if (k == 1) nums else {
        val vec = Vector[Int](0)
        find(1, vec, 1, Nil).reverse.toArray
      }
    }
  }
}
