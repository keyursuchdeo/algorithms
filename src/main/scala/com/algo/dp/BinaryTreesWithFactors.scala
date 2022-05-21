package com.algo.dp

object BinaryTreesWithFactors extends App {

  object Solution {
    def numFactoredBinaryTrees(arr: Array[Int]): Int = {
      val sortedNums = arr.sorted
      val nums: Map[Int, Int] = sortedNums.zipWithIndex.toMap
      val numOfTreesRootedAt: Array[Int] = new Array[Int](arr.length)


      @scala.annotation.tailrec
      def countNumOfBinaryTreesFor(index: Int, nextIndex: Int, count: Int): Int = {
        if (index == arr.length) {
          count
        } else if (nextIndex == arr.length) {
          countNumOfBinaryTreesFor(index + 1, index + 2, count + 1)
        } else {
          val remainder = sortedNums(nextIndex) / sortedNums(index)
          if (sortedNums(nextIndex) % sortedNums(index) == 0 && nums.contains(remainder)) {
            val additionalCount =
              if (remainder == sortedNums(index)) {
                1
              } else if (remainder < nums(index)) {
                0
              } else {
                2
              }
            numOfTreesRootedAt(nextIndex) =
              numOfTreesRootedAt(nextIndex) + additionalCount + numOfTreesRootedAt(index) + numOfTreesRootedAt(nums(remainder))
            countNumOfBinaryTreesFor(index, nextIndex + 1,
              count + additionalCount + numOfTreesRootedAt(index) + numOfTreesRootedAt(nums(remainder)))
          } else {
            countNumOfBinaryTreesFor(index, nextIndex + 1, count)
          }
        }
      }

      countNumOfBinaryTreesFor(0, 1, 0)
    }
  }

}
