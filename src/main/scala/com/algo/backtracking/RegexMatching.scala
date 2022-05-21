package com.algo.backtracking

object RegexMatching extends App {
  val s = "b"
  val p = "aaa."
  val res = Solution.isMatch(s, p)
  println(res)
}

object Solution {
  def isMatch(s: String, p: String): Boolean = {

    def matchAfterLastAsterisk(): (Boolean, String, String) = {
      val regexIndexOfLastAsterisk = p.lastIndexOf('*')
      if(regexIndexOfLastAsterisk == -1) {
        (true, s, p)
      } else {
        val (regexCharsUptoLastAsterisk, regexCharsAfterLastAsterisk) = p.splitAt(regexIndexOfLastAsterisk + 1)
        val regexNumOfCharsAfterLastAsterisk = regexCharsAfterLastAsterisk.length
        val (firstCharsOfS, lastCharsOfS) = s.splitAt(s.length - regexNumOfCharsAfterLastAsterisk)

        @scala.annotation.tailrec
        def m(index: Int, matchFound: Boolean): Boolean = {
          if (index == lastCharsOfS.length) {
            matchFound
          } else {
            if (regexCharsAfterLastAsterisk(index) == '.' || regexCharsAfterLastAsterisk(index) == lastCharsOfS(index)) {
              m(index + 1, matchFound = true)
            } else {
              false
            }
          }
        }
        if(regexNumOfCharsAfterLastAsterisk > s.length) {
          (false, s, p)
        } else (m(0, matchFound = true), firstCharsOfS, regexCharsUptoLastAsterisk)
      }
    }

    val (matchFound, updatedS, updatedP) = matchAfterLastAsterisk()

    def matchS(pIndex: Int, sIndex: Int, precedingChar: Option[Char]): Boolean = {
      if(sIndex == updatedS.length) {
        if(pIndex == updatedP.length || ifRemainingRegexMatchesEmptyString(pIndex)) {
          true
        } else {
          false
        }
      } else if(pIndex < updatedP.length && sIndex < updatedS.length) {
        updatedP(pIndex) match {
          case '.' =>
            matchS(pIndex + 1, sIndex + 1, Some('.'))
          case '*' =>
            precedingChar match {
              case None => false
              case Some(c) if c == '.' =>
                matchS(pIndex, sIndex + 1, precedingChar) || matchS(pIndex + 1, sIndex - 1, precedingChar)
              case Some(c) if c == updatedS(sIndex) =>
                matchS(pIndex, sIndex + 1, precedingChar)
              case Some(_) =>
                matchS(pIndex + 1, sIndex, None)
            }
          case x: Char if x == updatedS(sIndex) =>
            matchS(pIndex + 1, sIndex + 1, Some(x))
          case x: Char =>
            matchS(pIndex + 1, sIndex, Some(x))
        }
      } else {
        false
      }
    }

    def ifRemainingRegexMatchesEmptyString(pIndex: Int): Boolean = {
      val afterPIndex = updatedP.splitAt(pIndex)._2

      @scala.annotation.tailrec
      def ifMatchesEmptyString(index: Int, bool: Boolean): Boolean = {
        if(index == afterPIndex.length) {
          bool
        } else {
          afterPIndex(index) match {
            case '*' => ifMatchesEmptyString(index + 1, bool = true)
            case _ =>
              if (index + 1 == afterPIndex.length || afterPIndex(index + 1) != '*') {
                false
              } else {
                ifMatchesEmptyString(index + 2, bool = true)
              }
          }
        }
      }
      ifMatchesEmptyString(0, bool = false)
    }

    if(matchFound) matchS(0, 0, None) else false
  }
}