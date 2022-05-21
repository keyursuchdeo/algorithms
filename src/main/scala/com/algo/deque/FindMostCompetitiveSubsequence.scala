package com.algo.deque

object FindMostCompetitiveSubsequence extends App {
//  object Solution {
//    def mostCompetitive(nums: Array[Int], k: Int): Array[Int] = {
//      def find(index: Int, subSeq: Seq[Int], numOfSubSeqElements: Int, head: Int): Array[Int] = {
//        if(index == nums.length) {
//          subSeq.reverse.toArray
//        } else {
//          if(subSeq.isEmpty) {
//            find(index + 1, nums(index) +: subSeq, nums(index))
//          } else {
//            if(head >= nums(index)) {
//              if(nums.length - index - 1 >= k) {
//                find(index + 1, Seq(nums(index)), 1, nums(index))
//              } else {
//                find(index + 1, nums(index) +: subSeq, numOfSubSeqElements + 1, head)
//              }
//            } else {
//              if(nums.length - index - 1 >= k - numOfSubSeqElements) {
//                if(subSeq.head > nums(index)) {
//
//                }
//              }
//
//            }
//
//            if(nums.length - index - 1 >= k) {
//              if(head > nums(index)) {
//                find(index + 1, Seq(nums(index)), nums(index))
//              } else {
//                if(nums(index) > subSeq.head) {
//                  find(index + 1, subSeq, head)
//                } else if(nums(index) < subSeq.head) {
//                  find(index, subSeq.tail, head)
//                } else {
//                  find(index + 1, nums(index) +: subSeq, head)
//                }
//              }
//            } else {
//
//            }
//          }
//        }
//      }
//    }
//  }
}
