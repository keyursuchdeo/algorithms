package com.algo.arrays

object DecodedStringAtIndex2 extends App {

  val res = Solution.decodeAtIndex("leet2code32", 9)
  println(res)

  object Solution {
    def decodeAtIndex(S: String, K: Int): String = {
      val chars = S.toCharArray

      @scala.annotation.tailrec
      def calculateSizeOfDecodedString(index: Int, size: Long): Long = {
        if(index == chars.length) {
          size
        } else {
          if(chars(index).isLower) {
            calculateSizeOfDecodedString(index + 1, size + 1)
          } else {
            val num = chars(index).asDigit
            calculateSizeOfDecodedString(index + 1, size * num)
          }
        }
      }

      @scala.annotation.tailrec
      def find(index: Int, remainingK: Long, remainingSize: Long): String = {
        if(remainingK == 0 && chars(index).isLower) {
          chars(index).toString
        } else if(chars(index).isLower) {
          find(index - 1, remainingK % remainingSize, remainingSize - 1)
        } else {
          val num = chars(index).asDigit
          find(index - 1, remainingK % remainingSize, remainingSize / num)
        }
      }

      val decodedStrSize = calculateSizeOfDecodedString(0, 0)
      find(chars.length - 1, K % decodedStrSize, decodedStrSize)
    }
  }
}
