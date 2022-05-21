package com.algo.ufind

object RansomNote extends App {

  val ransomNote = "azc"
  val magazine = "acz"

  val res = Solution.canConstruct(ransomNote, magazine)
  println(res)

  object Solution {
    def canConstruct(ransomNote: String, magazine: String): Boolean = {
      val charCount = new Array[Int](26)

      def charIndex(char: Char): Int = {
        char - 'a'
      }

      @scala.annotation.tailrec
      def strCharCount(index: Int, str: String): Unit = {
        if(index == str.length) {
          ()
        } else {
          val strChar = str(index)
          val strCharIndex = charIndex(strChar)
          charCount(strCharIndex) += 1
          strCharCount(index + 1, str)
        }
      }

      @scala.annotation.tailrec
      def canNoteBeConstructed(note: String, index: Int): Boolean = {
        if (index == note.length) {
          true
        } else {
          val noteChar = note(index)
          val strCharIndex = charIndex(noteChar)
          if(charCount(strCharIndex) > 0) {
            charCount(strCharIndex) -= 1
            canNoteBeConstructed(note, index + 1)
          } else {
            false
          }
        }
      }

      strCharCount(0, magazine)
      println(charCount.mkString(","))
      canNoteBeConstructed(ransomNote, 0)

    }
  }
}
