package com.algo.arrays

object CheckIfStringContainsAllBinaryCodesOfSizeK extends App {

  object Solution {
    def hasAllCodes(s: String, k: Int): Boolean = {
      val chars = s.toCharArray
      val mid = (1 << k) / 2

      @scala.annotation.tailrec
      def check(lowIndex: Int, highIndex: Int, num: Int, codes: Set[Int]): Boolean = {
        if (highIndex == chars.length) {
          codes.isEmpty
        } else {
          val nextNum =
            if (chars(lowIndex) == '0') {
              if(chars(highIndex) == '0') {
                num * 2
              } else {
                num * 2 + 1
              }
            } else {
              if(chars(highIndex) == '0') {
                (num - mid) * 2
              } else {
                (num - mid) * 2 + 1
              }
            }
          check(lowIndex + 1, highIndex + 1, nextNum, codes - num)
        }
      }

      if(chars.length < k) {
        false
      } else {
        check(1, k, Integer.parseInt(s.take(k), 2), (0 until (1 << k)).toSet)
      }
    }
  }

}
