package com.algo.arrays

object KDiffPairsInArray1 extends App {

  val a = Array(3, 3, 1, 1)
  val d = 2

  val res = Solution.findPairs(a, d)
  println(res)

  object Solution {
    def findPairs(nums: Array[Int], k: Int): Int = {
      @scala.annotation.tailrec
      def mapNums(index: Int = 0, map: Map[Int, Int] = Map()): Map[Int, Int] = {
        if(index == nums.length) {
          map
        } else {
          map.get(nums(index)) match {
            case None =>
              mapNums(index + 1, map + (nums(index) -> 1))
            case Some(count) =>
              mapNums(index + 1, map + (nums(index) -> (1 + count)))
          }
        }
      }

      val map = mapNums(0, Map())
      map.keySet.count(key => {
        (k > 0 && map.contains(key + k)) || (k == 0 && map(key) > 1)
      })
    }
  }
}
