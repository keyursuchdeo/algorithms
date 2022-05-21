package com.algo.arrays

object NumbersAtMostNGivenDigitSet extends App {
//  object Solution {
//    def atMostNGivenDigitSet(digits: Array[String], n: Int): Int = {
//      val numOfDigits = digits.length
//      val numOfDigitsInN = n.toString.length
//      val totalNumsOfNDigits: Int = Math.pow(numOfDigits, numOfDigitsInN).toInt
//      lazy val sortedDigits = digits.sorted
//
//      def countNums(index: Int, count: Int, currN: Seq[Int]) = {
//        if(index == digits.length) {
//          count
//        } else {
//          if(sortedDigits(index).toInt < currN.head) {
//            countNums(index, count + totalNumsOfNDigits / numOfDigits, currN)
//          } else if(sortedDigits(index).toInt == currN.head) {
//            countNums(index, count, currN.tail)
//          }
//        }
//      }
//
//    }
//  }
}
