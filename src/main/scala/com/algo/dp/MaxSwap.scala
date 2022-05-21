package com.algo.dp

object MaxSwap extends App {
  val res = Solution.maximumSwap(1)
  println(res)

  object Solution {
    def maximumSwap(num: Int): Int = {
      val numChars = num.toString.toCharArray.zipWithIndex
      val nums = new Array[Int](numChars.length)
      nums(0) = num

      @scala.annotation.tailrec
      def swap(index: Int): Unit = {
        if(index == numChars.length) {
          ()
        } else {
          val currChar = numChars(index)._1
          numChars.find(charIndex => {
            val (char, charI) = charIndex
            charI < index && currChar > char
          }) match {
            case Some((_, charI)) =>
              nums(index) = prepNum(charI, index)
              swap(index + 1)
            case None =>
              nums(index) = num
              swap(index + 1)
          }
        }
      }

      def prepNum(from: Int, to: Int): Int = {
        val numCharArray: Array[Char] = num.toString.toCharArray
        val temp = numCharArray(from)
        numCharArray(from) = numCharArray(to)
        numCharArray(to) = temp
        numCharArray.mkString("").toInt
      }

      swap(1)
      println(nums.mkString(","))
      nums.max
    }
  }
}
