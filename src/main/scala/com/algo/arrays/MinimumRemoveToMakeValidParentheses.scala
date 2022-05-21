package com.algo.arrays

object MinimumRemoveToMakeValidParentheses extends App {
  object Solution {
    def minRemoveToMakeValid(s: String): String = {
      val chars = s.toCharArray

      @scala.annotation.tailrec
      def findParenIndicesToRemove(index: Int = 0, parenIndices: Seq[Int] = Nil): Seq[Int] = {
        if(index == chars.length) {
          parenIndices.reverse
        } else {
          if(chars(index).isLetter) {
            findParenIndicesToRemove(index + 1, parenIndices)
          } else if (chars(index) == '(') {
            findParenIndicesToRemove(index + 1, index +: parenIndices)
          } else {
            parenIndices.headOption match {
              case Some(parenIndex) if chars(parenIndex) == '(' =>
                findParenIndicesToRemove(index + 1, parenIndices.tail)
              case _ =>
                findParenIndicesToRemove(index + 1, index +: parenIndices)
            }
          }
        }
      }

      @scala.annotation.tailrec
      def formValidString(index: Int, stringBuilder: StringBuilder, parenIndices: Seq[Int]): String = {
        if(index == chars.length) {
          stringBuilder.toString()
        } else {
          parenIndices.headOption match {
            case Some(parenIndex) if parenIndex == index =>
              formValidString(index + 1, stringBuilder, parenIndices.tail)
            case _ =>
              formValidString(index + 1, stringBuilder.append(chars(index)), parenIndices)

          }
        }
      }

      val parenIndicesToRemove = findParenIndicesToRemove()
      if(parenIndicesToRemove.isEmpty) {
        s
      } else {
        formValidString(0, new StringBuilder(), parenIndicesToRemove)
      }

    }
  }
}
