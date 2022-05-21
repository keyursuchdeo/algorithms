package com.algo.arrays

import scala.collection.mutable

object PermutationsInString extends App {

  val s1 = "ab"
  val s2 = "eidboaoo"
  val res = Solution.checkInclusion(s1, s2)
  println(res)

  object Solution {
    def checkInclusion(s1: String, s2: String): Boolean = {
      if(s1.isEmpty || s2.isEmpty) {
        false
      } else {
        def charFrequency(str: String): Map[Char, Int] = {
          var map = mutable.Map[Char, Int]()
          for (c <- str) {
            map += (c -> (map.getOrElse(c, 0) + 1))
          }
          map.toMap
        }

        val charFreqS1 = charFrequency(s1)
        val s1Len = s1.length

        @scala.annotation.tailrec
        def find(l: Int, h: Int, currCharFreq: Map[Char, Int]): Boolean = {
          if (h >= s2.length) {
            false
          } else {
            if (currCharFreq.isEmpty) {
              val charFreqSubS: Map[Char, Int] = charFrequency(s2.substring(l, h + 1))
              println(charFreqSubS)
              if (charFreqSubS == charFreqS1) {
                true
              } else {
                find(l + 1, h + 1, charFreqSubS)
              }
            } else {
              if(s2(l - 1) == s2(h)) {
                if (currCharFreq == charFreqS1) {
                  true
                } else {
                  find(l + 1, h + 1, currCharFreq)
                }
              } else {
                val updatedFreqPrevChar: Int = currCharFreq(s2(l - 1)) - 1
                val updatedFreqNewChar: Int = currCharFreq.getOrElse(s2(h), 0) + 1
                val updatedCurrCharFreq =
                  if (updatedFreqPrevChar > 0) {
                    currCharFreq + (s2(l - 1) -> updatedFreqPrevChar) + (s2(h) -> updatedFreqNewChar)
                  } else {
                    currCharFreq - s2(l - 1) + (s2(h) -> updatedFreqNewChar)
                  }
                println(updatedCurrCharFreq)
                if (updatedCurrCharFreq == charFreqS1) {
                  true
                } else {
                  find(l + 1, h + 1, updatedCurrCharFreq)
                }
              }
            }

          }
        }

        find(0, s1Len - 1, Map())
      }
    }
  }
}
