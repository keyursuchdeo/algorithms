package com.algo.arrays

object RepeatedDNASequence extends App {

  val str = "AAAAAAAAAAA"
  val res = Solution.findRepeatedDnaSequences(str)
  println(res)

  object Solution {
    def findRepeatedDnaSequences(s: String): List[String] = {

      @scala.annotation.tailrec
      def find(currString: String, result: Seq[String], sequencesTillNow: Set[String]): Seq[String] = {
       if(currString.length < 10) {
         result
       } else {
         val sequence = currString.take(10)
         if(sequencesTillNow.contains(sequence)) {
           find(currString.tail, sequence +: result, sequencesTillNow)
         } else {
           find(currString.tail, result, sequencesTillNow + sequence)
         }
       }
      }

      if(s == null || s.length < 10) {
        Nil
      } else {
        find(s, Seq(), Set()).toList
      }
    }
  }
}
