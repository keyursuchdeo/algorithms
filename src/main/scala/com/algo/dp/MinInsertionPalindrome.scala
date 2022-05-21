package com.algo.dp

object MinInsertionPalindrome extends App {

  val res = Solution.minInsertions("abbaa")
  println(res)

  object Solution {
    def minInsertions(s: String): Int = {
      lazy val insertionCount = Array.fill[Int](s.length, s.length)(-1)
      lazy val chars = s.toCharArray

      def calculateInsertions(to: Int): Int = {
        val sub = chars.take(to + 1)

        def calculate(currSub: Array[Char], currFrom: Int, currTo: Int): Int = {
          if (currSub.length <= 1) {
            0
          } else if (insertionCount(currFrom)(currTo) != -1) {
            insertionCount(currFrom)(currTo)
          } else {
            val value =
              if (currSub.head == currSub(currSub.length - 1)) {
                calculate(currSub.take(currSub.length - 1).tail, currFrom + 1, currTo - 1)
              } else {
                1 + Math.min(
                  calculate(currSub.tail, currFrom + 1, currTo),
                  calculate(currSub.take(currSub.length - 1), currFrom, currTo - 1)
                )
              }
            insertionCount(currFrom)(currTo) = value
            value
          }
        }

        calculate(sub, 0, to)
      }

      @scala.annotation.tailrec
      def find(index: Int): Unit = {
        if (index == chars.length) {
          ()
        } else {
          calculateInsertions(index)
          find(index + 1)
        }
      }

      if (s.length == 1) 0 else find(1)
      println(insertionCount.map(_.mkString(",")).mkString("|"))
      insertionCount(0)(s.length - 1)
    }
  }

}
