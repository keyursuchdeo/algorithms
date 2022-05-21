package com.algo.arrays

import scala.util.{Success, Try}

object ReplaceQToAvoidRepeat extends App {
  object Solution {
    def modifyString(s: String): String = {
      val sChars = s.toCharArray

      @scala.annotation.tailrec
      def fillAvailableChars(char: Char, availableChars: Set[Char]): Set[Char] = {
        if(char > 'z') {
          availableChars
        } else {
          fillAvailableChars((char + 1).toChar, availableChars + char)
        }
      }

      @scala.annotation.tailrec
      def modify(index: Int, availableChars: Set[Char]): Unit = {
        if(index == sChars.length) {
          ()
        } else {
          if(sChars(index) == '?') {
            (Try(sChars(index - 1)), Try(sChars(index + 1)))  match {
              case (Success(bChar), Success(aChar)) =>
                sChars(index) = ((availableChars - bChar) - aChar).head
                modify(index + 1, availableChars)
              case (_, Success(aChar)) =>
                sChars(index) = (availableChars - aChar).head
                modify(index + 1, availableChars)
              case (Success(bChar), _) =>
                sChars(index) = (availableChars - bChar).head
                modify(index + 1, availableChars)
              case (_, _) =>
                sChars(index) = availableChars.head
                modify(index + 1, availableChars)
            }
          } else {
            modify(index + 1, availableChars)
          }
        }
      }

      modify(0, fillAvailableChars('a', Set()))
      sChars.mkString("")
    }
  }
}
