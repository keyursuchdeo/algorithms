package com.algo.arrays

object IntegerToRoman extends App {
  object Solution {
    val map: Map[Int, String] = Map[Int, String](
      1 -> "I",
      4 -> "IV",
      5 -> "V",
      9 -> "IX",
      10 -> "X",
      40 -> "XL",
      50 -> "L",
      90 -> "XC",
      100 -> "C",
      400 -> "CD",
      500 -> "D",
      900 -> "CM",
      1000 -> "M"
    )
    def intToRoman(num: Int): String = {
      @scala.annotation.tailrec
      def romanOf(num: Int, multiplier: Int, output: Seq[String]): String = {
        map.get(num) match {
          case Some(value) => (value +: output).mkString("")
          case None =>
            romanOf(num - 1 * multiplier, multiplier, map(1 * multiplier) +: output)
        }
      }

      @scala.annotation.tailrec
      def convert(remainingNum: Int, position: Int, output: Seq[String]): String = {
        if(remainingNum == 0) {
          output.mkString("")
        } else {
          val digit = remainingNum % 10
          if(digit == 0) {
            convert(remainingNum / 10, position + 1, output)
          } else {
            convert(remainingNum / 10, position * 1, romanOf(digit, position, Nil) +: output)
          }
        }
      }
      convert(num, 1, Nil)
    }
  }

}
