package com.algo.arrays

object VersionComparator extends App {
  object Solution {
    def compareVersion(version1: String, version2: String): Int = {
      val version1Parts: Seq[Int] = version1.split('.').map(_.toInt).toSeq
      val version2Parts: Seq[Int] = version2.split('.').map(_.toInt).toSeq

      @scala.annotation.tailrec
      def compare(currV1: Seq[Int], currV2: Seq[Int]): Int = {
        if(currV1.isEmpty && currV2.isEmpty) {
          0
        } else if (currV1.isEmpty) {
          if ((currV2.toSet - 0).isEmpty) 0 else -1
        } else if (currV2.isEmpty) {
          if ((currV1.toSet - 0).isEmpty) 0 else -1
        } else {
          if(currV1.head == currV2.head) {
            compare(currV1.tail, currV2.tail)
          } else if (currV1.head < currV2.head) {
            -1
          } else {
            1
          }
        }
      }

      compare(version1Parts, version2Parts)
    }
  }
}
