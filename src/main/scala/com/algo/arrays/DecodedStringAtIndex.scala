package com.algo.arrays

object DecodedStringAtIndex extends App {

  val res = Solution.decodeAtIndex("a2b3c4d5e6f7g8h9", 9)
  println(res)

  object Solution {
    def decodeAtIndex(S: String, K: Int): String = {
      val chars = S.toCharArray

      @scala.annotation.tailrec
      def decode(index: Int = 1, stringBeginIndex: Int = 0, stringEndIndex: Int = 0, frequency: Int = 0, remainingK: Int): String = {
        if(remainingK == 0) {
          chars(index - 1).toString
        } else {
          if(index == chars.length) {
            val strLength = stringEndIndex - stringBeginIndex + 1
            val repeatLength = strLength * (frequency - 1)
            if (remainingK - repeatLength == 0 || remainingK % strLength == 0) {
              chars(stringEndIndex).toString
            } else {
              chars(stringBeginIndex + (remainingK % strLength) - 1).toString
            }
          } else {
            if(chars(index).isLower) {
              if(frequency == 0) {
                decode(index + 1, stringBeginIndex, stringEndIndex, frequency,  remainingK - 1)
              } else {
                val strLength = stringEndIndex - stringBeginIndex + 1
                val repeatLength = strLength * (frequency - 1)
                if(remainingK - repeatLength > 0) {
                  decode(index + 1, index, index, 0, remainingK - repeatLength - 1)
                } else if (remainingK - repeatLength == 0 || remainingK % strLength == 0) {
                  chars(stringEndIndex).toString
                } else {
                  chars(stringBeginIndex + (remainingK % strLength) - 1).toString
                }
              }
            } else {
              val num = chars(index).asDigit
              if(frequency == 0) {
                decode(index + 1, stringBeginIndex, index - 1, num, remainingK)
              } else {
                decode(index + 1, stringBeginIndex, stringEndIndex, frequency * 10 + num, remainingK)
              }
            }
          }
        }
      }

      decode(remainingK = K - 1)
    }
  }
}
