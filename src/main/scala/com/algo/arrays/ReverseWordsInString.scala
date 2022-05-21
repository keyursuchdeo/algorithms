package com.algo.arrays

object ReverseWordsInString extends App {
  object Solution {
    def reverseWords(s: String): String = {
      s.split(" ").map(_.trim).filter(_.length > 0).reverse.mkString(" ")
    }
  }
}
