package com.algo.arrays

object LetterCasePermutation extends App {
  object Solution {
    def letterCasePermutation(S: String): List[String] = {
      val chars = S.toCharArray

      @scala.annotation.tailrec
      def findPermutations(index: Int, permutations: Seq[String] = Nil): Seq[String] = {
        if(index < 0) {
          permutations
        } else {
          if(permutations.isEmpty) {
            if(chars(index).isDigit) {
              findPermutations(index - 1, Seq(chars(index).toString))
            } else {
              findPermutations(index - 1,  Seq(chars(index).toLower.toString, chars(index).toUpper.toString))
            }
          } else {
            if(chars(index).isDigit) {
              findPermutations(index - 1, permutations.map(chars(index) +: _))
            } else {
              findPermutations(index - 1, permutations.flatMap(
                p => Seq(chars(index).toLower, chars(index).toUpper).map(_ +: p)))
            }
          }
        }
      }

      findPermutations(chars.length - 1).toList
    }
  }
}
