package com.algo.math

import java.util.stream.IntStream

object ReorderedPowerOf2 extends App {
  object Solution {
    def reorderedPowerOf2(N: Int): Boolean = {
      val digits: Array[Char] = N.toString.toCharArray
      val sortedDigits = digits.sorted
      (0 until 30).exists(power => {
        val powerDigits = (1 << power).toString.toCharArray
        powerDigits.sorted sameElements sortedDigits
      })
    }
  }
}
