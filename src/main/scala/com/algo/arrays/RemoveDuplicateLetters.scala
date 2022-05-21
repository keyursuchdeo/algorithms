package com.algo.arrays

object RemoveDuplicateLetters extends App {

  val str = "bbcaac"
  val res = Solution.removeDuplicateLetters(str)
  println(res)

  object Solution {
    def removeDuplicateLetters(s: String): String = {
      val chars = s.toCharArray
      val charIndices = Array.fill[Seq[Int]](26)(Nil)

      def getCharIndex(char: Char) = char - 'a'

      @scala.annotation.tailrec
      def fillCharIndices(index: Int): Unit = {
        if(index < 0) {
          ()
        } else {
          val charIndex = getCharIndex(chars(index))
          charIndices(charIndex) = index +: charIndices(charIndex)
          fillCharIndices(index - 1)
        }
      }

      def updateChars(chars: Seq[Char], char: Char, charSet: Set[Char]) = {
        @scala.annotation.tailrec
        def update(currChars: Seq[Char], currCharSet: Set[Char]): (Seq[Char], Set[Char]) = {
          if(currChars.isEmpty) {
            val charIndex = getCharIndex(char)
            charIndices(charIndex) = charIndices(charIndex).tail
            (Seq(char), (currCharSet + char))
          } else if (char > currChars.head && currCharSet.contains(char)) {
            (currChars, currCharSet)
          } else if (char > currChars.head) {
            val charIndex = getCharIndex(char)
            charIndices(charIndex) = charIndices(charIndex).tail
            (char +: currChars, currCharSet + char)
          } else {
            if(currCharSet.contains(char)) {
              val charIndex = getCharIndex(char)
              charIndices(charIndex) = charIndices(charIndex).tail
              (currChars, currCharSet)
            } else {
              val headCharIndex = getCharIndex(currChars.head)
              if(charIndices(headCharIndex) == Nil) {
                val charIndex = getCharIndex(char)
                charIndices(charIndex) = charIndices(charIndex).tail
                (char +: currChars, currCharSet + char)
              } else {
                update(currChars.tail, currCharSet - currChars.head)
              }
            }
          }
        }

        update(chars, charSet)
      }

      @scala.annotation.tailrec
      def prepOutputString(index: Int, uniqueChars: Seq[Char], charSet: Set[Char]): String = {
        println(uniqueChars)
        if(index == chars.length) {
          uniqueChars.reverse.mkString("")
        } else {
          val (updatedUniqueChars, updatedCharSet) = updateChars(uniqueChars, chars(index), charSet)
          prepOutputString(index + 1, updatedUniqueChars, updatedCharSet)
        }
      }

      fillCharIndices(s.length - 1)
      println(charIndices.mkString(","))
      prepOutputString(0, Nil, Set())
    }
  }
}
