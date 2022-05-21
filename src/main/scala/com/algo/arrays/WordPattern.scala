package com.algo.arrays

import scala.collection.mutable

object WordPattern extends App {
  object Solution {
    def wordPattern(pattern: String, str: String): Boolean = {
      def decodePattern(): Seq[Seq[Int]] = {
        val patternChars = pattern.toCharArray
        val charMap = new mutable.LinkedHashMap[Char, Seq[Int]]
        @scala.annotation.tailrec
        def decode(index: Int): Unit = {
          if(index == patternChars.length) {
            ()
          } else {
            charMap.get(patternChars(index)) match {
              case Some(indices) =>
                charMap += patternChars(index) -> (index +: indices)
                decode(index + 1)
              case None =>
                charMap += patternChars(index) -> Seq(index)
                decode(index + 1)
            }
          }
        }

        decode(0)
        charMap.values.toSeq
      }

      def decodeString(): Seq[Seq[Int]] = {
        val strWords = str.split(" ")
        val wordMap = new mutable.LinkedHashMap[String, Seq[Int]]
        @scala.annotation.tailrec
        def decode(index: Int): Unit = {
          if(index == strWords.length) {
            ()
          } else {
            wordMap.get(strWords(index)) match {
              case Some(indices) =>
                wordMap += strWords(index) -> (index +: indices)
                decode(index + 1)
              case None =>
                wordMap += strWords(index) -> Seq(index)
                decode(index + 1)
            }
          }
        }

        decode(0)
        wordMap.values.toSeq
      }

      decodePattern() == decodeString()
    }
  }
}
