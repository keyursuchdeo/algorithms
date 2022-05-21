package com.algo.arrays

object RemoveAllAdjacentDuplicatesInStringII extends App {

  val str = "deeedbbcccbdaa"
  val res = Solution.removeDuplicates(str, 3)
  println(res)

  object Solution {
    def removeDuplicates(s: String, k: Int): String = {
      val chars = s.toCharArray
      @scala.annotation.tailrec
      def remove(index: Int = 0, runningStack: Seq[(Char, Int)] = Nil): Seq[(Char, Int)] = {
        if(index == chars.length) {
          runningStack.reverse
        } else {
          runningStack.headOption match {
            case Some((char, count)) if char == chars(index) && count == k - 1 =>
              remove(index + 1, runningStack.tail)
            case Some((char, count)) if char == chars(index) =>
              remove(index + 1, (chars(index), count + 1) +: runningStack.tail)
            case _ =>
              remove(index + 1, (chars(index), 1) +: runningStack)
          }
        }
      }

      @scala.annotation.tailrec
      def prepString(runningStack: Seq[(Char, Int)], sb: StringBuilder): String = {
        if(runningStack.isEmpty) {
          sb.toString()
        } else {
          val (char, freq) = runningStack.head
          sb.appendAll((1 to freq).map(_ => char))
          prepString(runningStack.tail, sb)
        }
      }

      val remainingCharsAndCount = remove()
      prepString(remainingCharsAndCount, new StringBuilder())
    }
  }
}
