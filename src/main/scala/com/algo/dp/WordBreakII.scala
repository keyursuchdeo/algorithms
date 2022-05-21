package com.algo.dp

import scala.collection.mutable

object WordBreakII extends App {

  val str = "catsanddog"
  val d = List("cat","cats","and","sand","dog", "catsanddog", "c")
  val res = Solution.wordBreak(str, d)
  println(res)


  object Solution {
    def wordBreak(s: String, wordDict: List[String]): List[String] = {
      val dict = wordDict.toSet
      var map: mutable.Map[String, List[String]] = mutable.Map[String, List[String]]()

      def findWordBreaks(prefixS: String, suffixS: String, currBreaks: List[String], allBreaks: List[String]): List[String] = {
        if (suffixS.isEmpty) {
          if (prefixS.isEmpty) {
            currBreaks.reverse.mkString(" ") +: allBreaks
          } else {
            allBreaks
          }
        } else {
          val combinedS = s"$prefixS$suffixS"
          map.get(combinedS) match {
            case Some(words) if words.isEmpty => words
            case _ =>
              val currS = prefixS + suffixS.head
              val words =
                if (dict.contains(currS)) {
                  findWordBreaks("", suffixS.tail, currS +: currBreaks, allBreaks) ++
                    findWordBreaks(currS, suffixS.tail, currBreaks, allBreaks)
                } else {
                  findWordBreaks(currS, suffixS.tail, currBreaks, allBreaks)
                }
              map = map + (combinedS -> words)
              words
          }
        }
      }

      findWordBreaks("", s, Nil, Nil)
    }
  }

}
