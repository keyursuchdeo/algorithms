package com.algo.arrays

object FirstBadVersion extends App {

  val n = 2126753390
  val res = new Solution().firstBadVersion(n)
  println(res)

  abstract class VersionControl{
    def isBadVersion(version: Int): Boolean =
      version >= 1702766719
  }

  class Solution extends VersionControl {
    def firstBadVersion(n: Int): Int = {
      @scala.annotation.tailrec
      def find(l: Long, h: Long, badVersions: Seq[Int]): Seq[Int] = {
        println(s"l => $l, h => $h")
        if(h < l) {
          badVersions
        } else {
          val mid = Math.floor((l + h) / 2).toInt
          if(isBadVersion(mid)) {
            find(l, mid - 1, mid +: badVersions)
          } else {
            find(mid + 1, h, badVersions)
          }
        }
      }
      val bad = find(1, n, Nil)
      bad.headOption.getOrElse(-1)
    }
  }
}
