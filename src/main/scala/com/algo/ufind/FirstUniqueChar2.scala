package com.algo.ufind

object FirstUniqueChar2 extends App {

  val s = ""
  val res = Solution.firstUniqChar(s)
  println(res)

  object Solution {
    def firstUniqChar(s: String): Int = {

      val array: Array[Int] = new Array[Int](26)

      def getCharIndex(ch: Char): Int = {
        ch - 'a'
      }

      def recordCharFrequency(): Unit = {
        s.foreach(ch => {
          val charIndex = getCharIndex(ch)
          array(charIndex) += 1
        })
      }

//      @scala.annotation.tailrec
//      def findFirstUniqueChar(index: Int): Int = {
//        if (index == s.length) {
//          -1
//        } else {
//          val charIndex = getCharIndex(s(index))
//          if (array(charIndex) > 1) {
//            findFirstUniqueChar(index + 1)
//          } else {
//            index
//          }
//        }
//      }

      def findFirstUniqueChar(index: Int): Int = {
        s.zipWithIndex.find(chWithIndex => {
          val (ch, _) = chWithIndex
          val charIndex = getCharIndex(ch)
          array(charIndex) == 1
        }).map(_._2).getOrElse(-1)
      }

      recordCharFrequency()
      findFirstUniqueChar(0)
    }
  }
}
