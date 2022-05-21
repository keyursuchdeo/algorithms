package com.algo.ufind

import scala.collection.mutable

object LargestConsecutiveSequence extends App {

  val array = Array(100, 4, 200, 1, 3, 2)
  Solution.longestConsecutive(array)

  object Solution {
    def longestConsecutive(nums: Array[Int]): Int = {

      //for every number in array, find lower and higher number in set.
      //if not found, repeat for next number in the array
      //if found, add the number and lower and/or higher numbers to a new set

      def findConsecutiveNums(num: Int, consecutiveNumsSet: mutable.Set[Int], numsSet: mutable.Set[Int]): (mutable.Set[Int], mutable.Set[Int]) = {
        val (optS, optP, updatedNumsSet) = findPrecedingAndSucceedingNums(num, consecutiveNumsSet, numsSet)
        (optS, optP) match {
          case (Some(s), Some(p)) =>
            val (sConsesNumsSet, sUpdatedNumsSet) = findConsecutiveNums(s, consecutiveNumsSet + s, updatedNumsSet - num)
            val (pConsesNumsSet, pUpdatedNumsSet) = findConsecutiveNums(p, consecutiveNumsSet + p, updatedNumsSet - num)
            (sConsesNumsSet ++ pConsesNumsSet, sUpdatedNumsSet ++ pUpdatedNumsSet)
          case (None, Some(p)) =>
            findConsecutiveNums(p, consecutiveNumsSet + p, updatedNumsSet - num)
          case (Some(s), None) =>
            findConsecutiveNums(s, consecutiveNumsSet + s, updatedNumsSet - num)
          case (None, None) =>
            (consecutiveNumsSet, updatedNumsSet - num)
        }
      }

      def findPrecedingAndSucceedingNums(num: Int, consecutiveNumsSet: mutable.Set[Int], numsSet: mutable.Set[Int]): (Option[Int], Option[Int], mutable.Set[Int]) = {
        val succeeding: Option[Int] = if (!consecutiveNumsSet.contains(num + 1) && numsSet.contains(num + 1)) Option(num + 1) else None
        val preceding: Option[Int] = if (!consecutiveNumsSet.contains(num - 1) && numsSet.contains(num - 1)) Option(num - 1) else None

        (succeeding, preceding) match {
          case (Some(s), Some(p)) => (Some(s), Some(p), (numsSet - s) - p)
          case (None, Some(p)) => (None, Some(p), numsSet - p)
          case (Some(s), None) => (Some(s), None, numsSet - s)
          case (None, None) => (None, None, numsSet)
        }
      }


      @scala.annotation.tailrec
      def find(maxLength: Int, consecutiveNumsTillNow: mutable.Set[Int], numsSet: mutable.Set[Int]): Int = {
        val it = numsSet.iterator
        if(it.hasNext) {
          val num = it.next()
          val (consecutiveNums, updatedNumsSet) = findConsecutiveNums(num, mutable.Set(num), numsSet)
          if(consecutiveNums.size > maxLength) {
            find(consecutiveNums.size, consecutiveNumsTillNow ++ consecutiveNums, updatedNumsSet)
          } else {
            find(maxLength, consecutiveNumsTillNow ++ consecutiveNums, updatedNumsSet)
          }
        } else {
          maxLength
        }
      }

      val a = find(0, mutable.Set(), mutable.Set() ++= nums)
//      println(a)
      a
    }
  }
}
