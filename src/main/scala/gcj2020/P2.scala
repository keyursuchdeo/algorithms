package gcj2020

import scala.io.StdIn

object P2 extends App {

  new Solution().main()

  class Solution {
    def main(): Unit = {
      val numOfTestCases: Int = StdIn.readInt()
      for (x <- 1 to numOfTestCases) {
        val string = StdIn.readLine()
        val pString = addP(string)
        println(s"Case #$x: $pString")
      }
    }

    private def addP(string: String): String = {
      val stringBuilder = new StringBuilder()

      @scala.annotation.tailrec
      def add(index: Int, currOpenP: Int): String = {
        if (index == string.length) {
          appendClosingP(currOpenP, stringBuilder)
          stringBuilder.toString()
        } else {
          val currDigit = string(index).toString.toInt
          if (currDigit == currOpenP) {
            stringBuilder.append(string(index))
            add(index + 1, currOpenP)
          } else if (currDigit > currOpenP) {
            appendOpeningP(currDigit - currOpenP, stringBuilder)
            stringBuilder.append(currDigit)
            add(index + 1, currDigit)
          } else {
            appendClosingP(currOpenP - currDigit, stringBuilder)
            stringBuilder.append(currDigit)
            add(index + 1, currDigit)
          }
        }
      }

      add(0, 0)
    }

    private def appendOpeningP(count: Int, sb: StringBuilder): Unit =
      (0 until count).foreach(_ => sb.append("("))

    private def appendClosingP(count: Int, sb: StringBuilder): Unit =
      (0 until count).foreach(_ => sb.append(")"))
  }
}
