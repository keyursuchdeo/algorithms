package com.algo.heap


object SortByFreq extends App {

  val str = ""
  val res = Solution.frequencySort(str)
  println(res)

  object Solution {
    def frequencySort(s: String): String = {

      @scala.annotation.tailrec
      def calculateCharFreq(index: Int, charFrequencies: Map[Char, Int] = Map()): Map[Char, Int] = {
        if(index == s.length) {
          charFrequencies
        } else {
          charFrequencies.get(s(index)) match {
            case Some(freq) =>
              calculateCharFreq(index + 1, charFrequencies + (s(index) -> (1 + freq)))
            case _ =>
              calculateCharFreq(index + 1, charFrequencies + (s(index) -> 1))
          }
        }
      }

      def prepStr(charFrequency: (Char, Int)): String = {
        charMultiplier(charFrequency._1, charFrequency._2)
      }

      def charMultiplier(char: Char, count: Int): String = {
        val sb: StringBuilder = new StringBuilder()
        (1 to count).foreach(_ => sb.append(char))
        sb.toString()
      }

      calculateCharFreq(0).toSeq.sortBy(_._2).reverse.map(prepStr).mkString("")

    }
  }
}
