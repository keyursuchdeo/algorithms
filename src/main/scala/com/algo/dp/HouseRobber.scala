package com.algo.dp

object HouseRobber extends App {
  val a = Array(1)
  val res = Solution.rob(a)
  println(res)

  object Solution {
    def rob(nums: Array[Int]): Int = {
      if (nums.isEmpty) {
        0
      } else if (nums.length == 1) {
        nums(0)
      } else {
        Math.max(
          Math.max(calculatedRobbedMoney(0, nums),
            calculatedRobbedMoney(1, nums)),
          calculatedRobbedMoney(nums.length - 1, nums)
        )
      }
    }

    def calculatedRobbedMoney(startingHouse: Int, nums: Array[Int]): Int = {
      val numsWithIndex = nums.zipWithIndex
      if (startingHouse == 0) {
        numsWithIndex.collect {
          case (num, index) if index % 2 == 0 && !isLastIndex(index, nums) => num
        }.sum
      } else if (startingHouse == 1) {
        numsWithIndex.collect{
          case (num, index) if index % 2 == 1 => num
        }.sum
      } else if (isLastIndex(startingHouse, nums)) {
        numsWithIndex.collect {
          case (num, index) if index % 2 == 0 && index > 0 => num
        }.sum
      } else {
        0
      }
    }

    def isLastIndex(index: Int, nums: Array[Int]): Boolean = {
      index == nums.length - 1
    }
  }
}
