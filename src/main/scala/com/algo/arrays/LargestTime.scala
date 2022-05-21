package com.algo.arrays

object LargestTime extends App {
  object Solution {
    def largestTimeFromDigits(A: Array[Int]): String = {

      def possibleCombinations(currA: Array[Int]): Seq[String] = {
        if(currA.tail.isEmpty) {
          Seq(currA.head.toString)
        } else {
          currA.indices.flatMap(index => {
            val (before, after) = currA.splitAt(index)
            possibleCombinations(before ++ after.tail).map(c => (currA(index) + c.toString))
          })
        }
      }

      def filterValidCombinations(combinations: Seq[String]): Seq[String] = {
        combinations.filter(c => {
          c.toInt < 2400 && c.toInt % 100 < 60
        })
      }

      val validCombinations = filterValidCombinations(possibleCombinations(A))
      if(validCombinations.isEmpty) {
        ""
      } else {
        val (before, after) = validCombinations.max.splitAt(2)
        s"$before:$after"
      }
    }
  }
}
