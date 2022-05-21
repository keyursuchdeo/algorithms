package com.algo.arrays

object AmbiguousCoordinates extends App {

  object Solution {
    def ambiguousCoordinates(s: String): List[String] = {

      def prepNumbers(num: String) = {
        val numChars = num.toCharArray

        @scala.annotation.tailrec
        def prep(index: Int, nums: Seq[String]): Seq[String] = {
          if (index == numChars.length) {
            num +: nums
          } else {
            val (left, right) = numChars.splitAt(index + 1)
            if (right.length == 0 || (right(right.length - 1) == '0')) {
              prep(index + 1, nums)
            } else {
              val possibleNum = left.mkString("") + "." + right.mkString("")
              prep(index + 1, possibleNum +: nums)
            }
          }
        }

        if((num.toInt == 0 && numChars.length > 1) || (numChars(numChars.length - 1) == '0' && numChars.length > 1)) {
          Nil
        } else if(numChars.length == 1) {
          Seq(num)
        } else if (numChars(0) == '0') {
          Seq(s"0.${num.tail}")
        } else {
          prep(0, Nil)
        }
      }


      def findPossibilities(): Seq[String] = {
        val chars = s.tail.toCharArray.dropRight(1)
        @scala.annotation.tailrec
        def find(index: Int, output: Seq[String]): Seq[String] = {
          if(index == chars.length) {
            output
          } else {
            val (left, right) = chars.splitAt(index + 1)
            if(left.length > 0 && right.length > 0) {
              val leftNums: Seq[String] = prepNumbers(left.mkString(""))
              println(leftNums)
              val rightNums: Seq[String] = prepNumbers(right.mkString(""))
              println(rightNums)
              val addlOutput: Seq[String] = leftNums.flatMap(leftNum => rightNums.map(rightNum => {
                s"($leftNum, $rightNum)"
              }))
              find(index + 1, output ++ addlOutput)
            } else {
              find(index + 1, output)
            }
          }
        }
        find(0, Nil)
      }

      findPossibilities().toList
    }
  }

}
