package com.algo.dp

//object MaxConsecutiveOnesIII extends App {
//  object Solution {
//    def longestOnes(nums: Array[Int], k: Int): Int = {
//      def maxConsecutiveOnes(index: Int, remainingK: Int): Int = {
//        if(index == nums.length) {
//          0
//        } else {
//          if (nums(index) == 1) {
//            1 + maxConsecutiveOnes(index + 1, remainingK)
//          } else {
//            if(remainingK == 0) {
//              0
//            } else {
//              Math.max(
//                1 + maxConsecutiveOnes(index + 1, remainingK - 1),
//
//              )
//            }
//          }
//        }
//      }
//
//      maxConsecutiveOnes(0, k)
//    }
//  }
//}
