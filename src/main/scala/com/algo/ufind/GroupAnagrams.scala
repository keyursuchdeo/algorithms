package com.algo.ufind

import scala.collection.mutable

object GroupAnagrams extends App {
  val strs = Array("eatea", "eat")
  val res = Solution.groupAnagrams(strs)
  println(res)

  object Solution {
    def groupAnagrams(strs: Array[String]): List[List[String]] = {
      val anagrams = Array.fill[List[String]](strs.length)(Nil)

      @scala.annotation.tailrec
      def group(index: Int, anagramSets: Map[Map[Char, Int], Int], currIndex: Int): Unit = {
        if(index == strs.length) {
          ()
        } else {
          findAnagramSet(strs(index), anagramSets) match {
            case Left(aIndex) =>
              anagrams(aIndex) = strs(index) +: anagrams(aIndex)
              group(index + 1, anagramSets, currIndex)
            case Right(charMap) =>
              anagrams(currIndex) = List(strs(index))
              group(index + 1, anagramSets + (charMap -> currIndex), currIndex + 1)
          }
        }
      }

      def findAnagramSet(str: String, anagramSets: Map[Map[Char, Int], Int]): Either[Int, Map[Char, Int]] = {
        val charMap = charCount(str)
        anagramSets.get(charMap) match {
          case Some(index) => Left(index)
          case None => Right(charMap.toMap)
        }
      }

      def charCount(str: String): Map[Char, Int] = {
        var map = mutable.Map[Char, Int]()
        str.foreach(char => {
          map += (char -> (map.getOrElse(char, 0) + 1))
        })
        map.toMap
      }

      group(0, Map(), 0)
      anagrams.toList.filter(_.nonEmpty)
    }
  }
}
