package com.algo.dp

import java.util

import scala.collection.mutable

object ContiguousArray2 extends App {

  val a = Array(0, 0, 1, 0, 0, 0, 1, 1)
//  val a = Array(0,1,1,0,1,1,1,0)
  val res = Solution.findMaxLength(a)
  println(res)

  object Solution {
    def findMaxLength(nums: Array[Int]): Int = {
      var map: util.HashMap[Int, Int] = new java.util.HashMap[Int, Int]
      map.put(0, -1)
      @scala.annotation.tailrec
      def find(index: Int, currCount: Int, maxLen: Int): Int = {
        if(index == nums.length) {
          maxLen
        } else {
          val newCount = if(nums(index) == 0) currCount - 1 else currCount + 1
          if(map.containsKey(newCount)) {
            find(index + 1, newCount, Math.max(maxLen, index - map.get(newCount)))
          } else {
            map.put(newCount, index)
            find(index + 1, newCount, maxLen)
          }
//          map.get(newCount) match {
//            case None =>
////              println(s"$index $map $maxLen")
//              map.put(newCount, index)
//              find(index + 1, newCount, maxLen)
//            case Some(currIndex) =>
////              println(s"$index $map $newMaxLen")
//              find(index + 1, newCount, Math.max(maxLen, index - currIndex))
//
//          }
        }
      }
      find(0, 0, 0)
//      println(map)
    }
  }
}
