package com.algo.dp

object PalindromePartitioning extends App {

  val res = Solution.partition("ab")
  println(res)

  object Solution {
    def partition(s: String): List[List[String]] = {
      var output = Seq[List[String]]()

      def isPalindrome(str: String): Boolean = {
        str == str.reverse
      }

      def find(before: String, after: String, palindromes: List[List[String]]): List[List[String]] = {
        if (after.isEmpty) {
          if (isPalindrome(before)) {
            List(before) +: palindromes
          } else {
            palindromes
          }
        } else {
          if (isPalindrome(before) && isPalindrome(after)) {
            val a = find(before + after.head, after.tail, List(before, after) +: palindromes)
            val b = find(after.head.toString, after.tail, Nil) match {
              case Nil => Nil
              case xs => xs.map(before +: _)
            }
            a ++ b
          } else if (isPalindrome(before)) {
            val a: List[List[String]] = find(before + after.head, after.tail, palindromes)
            val b: List[List[String]] = find(after.head.toString, after.tail, Nil) match {
              case Nil => Nil
              case xs => xs.map(before +: _)
            }
            a ++ b
          } else {
            find(before + after.head, after.tail, palindromes)
          }
        }
      }

      if (s.isEmpty) {
        List(Nil)
      } else {
        find(s.head.toString, s.tail, Nil).distinct
      }
    }
  }

}
