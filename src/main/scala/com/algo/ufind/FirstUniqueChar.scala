package com.algo.ufind

object FirstUniqueChar extends App {

  val s = "loveleetcode"
  val res = Solution.firstUniqChar(s)
  println(res)

  object Solution {
    def firstUniqChar(s: String): Int = {

      @scala.annotation.tailrec
      def findUniqueChar(set: Set[Char], subS: String, index: Int): Int = {
        subS.headOption match {
          case Some(head) =>
            if(set.contains(head)) {
              findUniqueChar(set, subS.tail, index + 1)
            } else {
              if (subS.tail.contains(head)) {
                findUniqueChar(set + head, subS.tail, index + 1)
              } else {
                index
              }
            }
          case None => -1
        }
      }

      findUniqueChar(Set(), s, 0)
    }
  }
}
