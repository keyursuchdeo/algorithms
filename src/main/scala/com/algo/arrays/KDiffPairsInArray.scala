package com.algo.arrays

object KDiffPairsInArray extends App {

  val a = Array(3, 3, 1, 1)
  val d = 2

  val res = Solution.findPairs(a, d)
  println(res)

  object Solution {
    def findPairs(nums: Array[Int], k: Int): Int = {

      @scala.annotation.tailrec
      def mapNums(index: Int = 0, map: Map[Int, Seq[Int]] = Map()): Map[Int, Seq[Int]] = {
        if(index == nums.length) {
          map
        } else {
          map.get(nums(index)) match {
            case None =>
              mapNums(index + 1, map + (nums(index) -> Seq(index)))
            case Some(indices) =>
              mapNums(index + 1, map + (nums(index) -> (index +: indices)))
          }
        }
      }

      def updateMapAndCount(map: Map[Int, Seq[Int]], num: Int, diff: Int, set: Set[Seq[Int]]): (Map[Int, Seq[Int]], Int, Set[Seq[Int]]) = {
        map.get(diff) match {
          case None =>
            (map, 0, set)
          case Some(_) if set.contains(Seq(num, diff)) =>
            (map, 0, set)
          case Some(_) if set.contains(Seq(diff, num)) =>
            (map, 0, set)
          case Some(indices) =>
            val updatedIndices1 = indices.tail
            val updatedIndices2 = map(num).tail
            val updatedMap1 =
              if(updatedIndices1.isEmpty) {
                map - diff
              } else {
                map + (diff -> updatedIndices1)
              }
            val updatedMap2 =
              if(updatedIndices2.isEmpty) {
                updatedMap1 - num
              } else {
                updatedMap1 + (num -> updatedIndices2)
              }
            (updatedMap2, 1, set + Seq(num, diff))
        }
      }

      @scala.annotation.tailrec
      def find(index: Int, map: Map[Int, Seq[Int]], count: Int, set: Set[Seq[Int]]): Int = {
        if(index == nums.length) {
          count
        } else {
          val num = nums(index)
          val (updatedMap, countDelta, updatedSet) = updateMapAndCount(map, num, num - k, set)
          if(countDelta == 0) {
            val (updatedMap1, countDelta1, updatedSet1) = updateMapAndCount(map, num, k + num, set)
            find(index + 1, updatedMap1, count + countDelta1, updatedSet1)
          } else {
            find(index + 1, updatedMap, count + countDelta, updatedSet)
          }
        }
      }

      find(0, mapNums(), 0, Set())
    }
  }
}
