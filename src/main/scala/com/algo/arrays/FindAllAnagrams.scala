package com.algo.arrays

import scala.collection.mutable

object FindAllAnagrams extends App {

  val sStr = ""
  val pStr = ""
  val res = Solution.findAnagrams(sStr, pStr)
  println(res)

  object Solution {
    def findAnagrams(s: String, p: String): List[Int] = {
      if(s.isEmpty || p.isEmpty) {
        Nil
      } else {
        def charFrequency(str: String): Map[Char, Int] = {
          var map = mutable.Map[Char, Int]()
          for (c <- str) {
            map += (c -> (map.getOrElse(c, 0) + 1))
          }
          map.toMap
        }

        val charFreqP = charFrequency(p)
        val pLen = p.length

        @scala.annotation.tailrec
        def find(l: Int, h: Int, anagramIndices: Seq[Int], currCharFreq: Map[Char, Int]): Seq[Int] = {
          if (h >= s.length) {
            anagramIndices
          } else {
            if (currCharFreq.isEmpty) {
              val charFreqSubS: Map[Char, Int] = charFrequency(s.substring(l, h + 1))
              println(charFreqSubS)
              if (charFreqSubS == charFreqP) {
                find(l + 1, h + 1, l +: anagramIndices, charFreqSubS)
              } else {
                find(l + 1, h + 1, anagramIndices, charFreqSubS)
              }
            } else {
              if(s(l - 1) == s(h)) {
                if (currCharFreq == charFreqP) {
                  find(l + 1, h + 1, l +: anagramIndices, currCharFreq)
                } else {
                  find(l + 1, h + 1, anagramIndices, currCharFreq)
                }
              } else {
                val updatedFreqPrevChar: Int = currCharFreq(s(l - 1)) - 1
                val updatedFreqNewChar: Int = currCharFreq.getOrElse(s(h), 0) + 1
                val updatedCurrCharFreq =
                  if (updatedFreqPrevChar > 0) {
                    currCharFreq + (s(l - 1) -> updatedFreqPrevChar) + (s(h) -> updatedFreqNewChar)
                  } else {
                    currCharFreq - s(l - 1) + (s(h) -> updatedFreqNewChar)
                  }
                println(updatedCurrCharFreq)
                if (updatedCurrCharFreq == charFreqP) {
                  find(l + 1, h + 1, l +: anagramIndices, updatedCurrCharFreq)
                } else {
                  find(l + 1, h + 1, anagramIndices, updatedCurrCharFreq)
                }
              }
            }

          }
        }

        find(0, pLen - 1, Nil, Map()).reverse.toList
      }
    }
  }

}
