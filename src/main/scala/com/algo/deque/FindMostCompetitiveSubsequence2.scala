package com.algo.deque

object FindMostCompetitiveSubsequence2 extends App {
  object Solution {
    def mostCompetitive(nums: Array[Int], k: Int): Array[Int] = {
      @scala.annotation.tailrec
      def find(index: Int, output: Seq[Int], outputLen: Int): Array[Int] = {
        if(index == nums.length) {
          output.toArray.reverse
        } else {
          if(output.isEmpty) {
            find(index + 1, nums(index) +: output, outputLen + 1)
          } else {
            if(nums(index) < output.head) {
              if(nums.length - index - 1 >= k - outputLen) {
                find(index, output.tail, outputLen - 1)
              } else {
                find(index + 1, nums(index) +: output, outputLen + 1)
              }
            } else {
              if(outputLen < k) {
                find(index + 1, nums(index) +: output, outputLen + 1)
              } else {
                find(index + 1, output, outputLen)
              }
            }
          }
        }
      }

      find(0, Nil, 0)
    }
  }
}
