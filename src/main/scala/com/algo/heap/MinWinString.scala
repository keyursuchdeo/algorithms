package com.algo.heap

import scala.collection.mutable

object MinWinString extends App {

  val s = "ADOBECODEBANC"
  val t = "ABC"

  Solution.minWindow(s, t)

  object Solution {
    def minWindow(s: String, t: String): String = {

      @scala.annotation.tailrec
      def min(index: Int, vec: Vector[(Char, Int)], vecLen: Int, sb: mutable.StringBuilder, winStringWithLenSeq: Seq[String]): Seq[String] = {
        if (index == s.length) {
          winStringWithLenSeq
        } else {
          val updatedSb: StringBuilder = sb.append(s(index))
          if(t.contains(s(index))) {
            //TODO - fails on duplicate chars
            if (vecLen + 1 == t.length) { // all chars found in queue
              val winString = updatedSb.toString()
              val updatedVecAfterDrop = (vec :+ (s(index), index)).drop(1)
              val (_, dropTillIndex) = updatedVecAfterDrop.head
              min(index + 1, updatedVecAfterDrop, vecLen, updatedSb.drop(dropTillIndex), winString +: winStringWithLenSeq)
            } else {
              val updatedVecAfterAdd = (vec :+ (s(index), index))
              min(index + 1, updatedVecAfterAdd, vecLen + 1, updatedSb, winStringWithLenSeq)
            }
          } else {
            min(index + 1, vec, vecLen, updatedSb, winStringWithLenSeq)
          }
        }
      }

      val minStrings = min(0, Vector(), 0, new mutable.StringBuilder(), Nil)
      println(minStrings)
      minStrings.headOption.getOrElse("")
    }
  }
}
