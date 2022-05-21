package com.algo.micro

object Ocr extends App {

  val s = "A2Le"
  val t = "2pL1"

//  val s = "3x2x"
//  val t = "7"

  Solution.solution(s, t)

  object Solution {
    def solution(s: String, t: String): Boolean = {
      val expandedS = replaceNumByUnrecognizedChar(s)
      val expandedT = replaceNumByUnrecognizedChar(t)
      if(expandedS.length != expandedT.length) false else ifMatching(expandedS, expandedT)
    }

    private def replaceNumByUnrecognizedChar(string: String): String = {
      val stringBuilderAlphabets = new StringBuilder(string.length)
      val stringBuilderNums = new StringBuilder()
      for (c <- string) {
        if(c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' || c == '7' || c == '8' || c == '9') {
          stringBuilderNums.append(c)
        } else {
          val numSeq = stringBuilderNums.toString()
          val unrecognizedChars = if(numSeq.nonEmpty) convertNumSeqToUnrecognizedCharSeq(numSeq) else ""
          stringBuilderAlphabets.append(unrecognizedChars)
          stringBuilderAlphabets.append(c)
          stringBuilderNums.clear()
        }
      }
      val numSeq = stringBuilderNums.toString()
      val unrecognizedChars = if(numSeq.nonEmpty) convertNumSeqToUnrecognizedCharSeq(numSeq) else ""
      stringBuilderAlphabets.append(unrecognizedChars)

      stringBuilderAlphabets.toString()
    }

    private def convertNumSeqToUnrecognizedCharSeq(numSeq: String): String = {
      val num = numSeq.toInt
      val stringBuilder = new StringBuilder(numSeq.length)
      (0 until num).foreach(_ => stringBuilder.append('?'))
      stringBuilder.toString()
    }

    private def ifMatching(s: String, t: String): Boolean = {
      @scala.annotation.tailrec
      def identifyUnrecognizedChars(index: Int, correctedS: StringBuilder, correctedT: StringBuilder): (String, String) = {
        if (index == s.length) {
          (correctedS.toString(), correctedT.toString())
        } else {
          if (s(index) != t(index)) {
            if (s(index) == '?') {
              identifyUnrecognizedChars(index + 1, correctedS.append(t(index)), correctedT.append(t(index)))
            } else if (t(index) == '?') {
              identifyUnrecognizedChars(index + 1, correctedS.append(s(index)), correctedT.append(s(index)))
            } else {
              identifyUnrecognizedChars(index + 1, correctedS.append(s(index)), correctedT.append(t(index)))
            }
          } else {
            identifyUnrecognizedChars(index + 1, correctedS.append(s(index)), correctedT.append(t(index)))
          }
        }
      }

      val (correctedS, correctedT) =
        identifyUnrecognizedChars(0, new StringBuilder(s.length), new StringBuilder(t.length))

      correctedS == correctedT
    }
  }
}
