package com.algo.arrays

object MaximumErasureValue extends App {
  val a = Array(1, 2, 1, 2)
  val res: Int = Solution.maximumUniqueSubarray(a)
  println(res)

  object Solution {
    def maximumUniqueSubarray(nums: Array[Int]): Int = {
      var seenNums: Map[Int, Int] = Map()
      var ans = nums(0)
      @scala.annotation.tailrec
      def find(index: Int, startingIndex: Int, sum: Int): Int = {
        if(index == nums.length) {
          Math.max(ans, sum)
        } else {
          seenNums.get(nums(index)) match {
            case Some(prevIndex) =>
              ans = Math.max(ans, sum)
              val sumToDeduct = (startingIndex to prevIndex).map(i => {
                seenNums = seenNums - nums(i)
                nums(i)
              }).sum
              seenNums = seenNums + (nums(index) -> index)
              println(seenNums)
              find(index + 1, prevIndex + 1, sum - sumToDeduct + nums(index))
            case _ =>
              seenNums = seenNums + (nums(index) -> index)
              find(index + 1, startingIndex, sum + nums(index))
          }
        }
      }

      find(index = 1, startingIndex = 0, nums(0))
    }
  }
}
