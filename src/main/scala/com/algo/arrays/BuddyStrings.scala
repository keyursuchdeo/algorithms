package com.algo.arrays

object BuddyStrings extends App {
  object Solution {
    def buddyStrings(A: String, B: String): Boolean = {

      if(A.length == B.length) {
        if(A == B) {
          val charsA = A.toCharArray.toSeq
          val charFrequencies = new Array[Int](26)
          def getCharIndex(char: Char) = char - 'a'
          @scala.annotation.tailrec
          def charFrequency(index: Int): Boolean = {
            if(index == charsA.length) {
              charFrequencies.exists(_ > 1)
            } else {
              val charIndex = getCharIndex(charsA(index))
              charFrequencies(charIndex) = charFrequencies(charIndex) + 1
              charFrequency(index + 1)
            }
          }
          charFrequency(0)
        } else {
          val charsA = A.toCharArray.toSeq
          val charsB = B.toCharArray.toSeq

          @scala.annotation.tailrec
          def check(index: Int, differencesTillNow: Int, optDiffIndex: Option[Int]): Boolean = {
            if(index == charsA.length) {
              differencesTillNow == 2
            } else {
              val charA = charsA(index)
              val charB = charsB(index)
              if(charA == charB) {
                check(index + 1, differencesTillNow, optDiffIndex)
              } else {
                optDiffIndex match {
                  case Some(diffIndex) if differencesTillNow == 1 =>
                    val diffCharA = charsA(diffIndex)
                    val diffCharB = charsB(diffIndex)
                    if(diffCharA == charB && diffCharB == charA) {
                      check(index + 1, differencesTillNow + 1, optDiffIndex)
                    } else {
                      false
                    }
                  case Some(_) =>
                    false
                  case None =>
                    check(index + 1, differencesTillNow + 1, Option(index))
                }
              }
            }
          }

          check(0, 0, None)
        }

      } else {
        false
      }
    }
  }
}
