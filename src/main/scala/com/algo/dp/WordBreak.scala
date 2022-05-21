package com.algo.dp

import scala.collection.mutable

object WordBreak extends App {

//  val s = "leetcode"
//  val wordDict = List("leet", "code")
//  val s = "applepenapple"
//  val wordDict = List("apple", "pen")
//    val s = "catsandog"
//    val wordDict = List("cats", "dog", "sand", "and", "cat")

  val s = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
  val wordDict = List("a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa")

//  val s = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
//  val wordDict = List("a","aa","ba")

  val res = Solution.wordBreak(s, wordDict)
  println(res)

  object Solution {
    def wordBreak(s: String, wordDict: List[String]): Boolean = {
      if (wordDict.isEmpty) false else {
        val dict: Set[String] = wordDict.toSet
        val dictWordMaxLen = wordDict.map(_.length).max
        var map: mutable.Map[(Int, Int), Boolean] = mutable.Map[(Int, Int), Boolean]()

        def find(prevWordBreakIndex: Int, currIndex: Int): Boolean = {
          if (currIndex == s.length) {
            prevWordBreakIndex == currIndex
          } else {
            map.get((prevWordBreakIndex, currIndex)) match {
              case Some(true) =>
                find(currIndex + 1, currIndex + 1)
              case Some(false) =>
                false
              case _ =>
                val subString = s.substring(prevWordBreakIndex, currIndex + 1)
                if (dictWordMaxLen < subString.length) {
                  map = map + ((prevWordBreakIndex, currIndex) -> false)
                  false
                } else {
                  if (dict.contains(subString)) {
                    println(s"$subString true $currIndex $prevWordBreakIndex")
                    map = map + ((prevWordBreakIndex, currIndex) -> true)
                    find(currIndex + 1, currIndex + 1) || find(prevWordBreakIndex, currIndex + 1)
                  } else {
                    println(s"$subString false $currIndex $prevWordBreakIndex")
                    map = map + ((prevWordBreakIndex, currIndex) -> false)
                    find(prevWordBreakIndex, currIndex + 1)
                  }
                }
            }

          }
        }

        val a = find(0, 0)
        println(map)
        a
      }
    }
  }
}
