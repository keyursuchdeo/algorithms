package com.algo.dp

object SubArraySum extends App {

  val a = Array(28,54,7,-70,22,65,-6)
  val k = 100
  val res = Solution.subarraySum(a, k)
  println(res)

  object Solution {
    def subarraySum(nums: Array[Int], k: Int): Int = {

      @scala.annotation.tailrec
      def find(index: Int, count: Int, currSum: Option[Int], nextStartIndex: Int): Int = {
        if(index >= nums.length) {
          count
        } else {
          addTillK(currSum, nextStartIndex) match {
            case (Some(sum), nextStartIndex) if sum == k =>
              find(index + 1, count + 1, Option(sum - nums(index)), nextStartIndex)
            case (Some(sum), nextStartIndex) =>
              find(index + 1, count, Option(sum - nums(index)), nextStartIndex)
            case (None, _) =>
              count
          }
        }
      }

      def addTillK(currSum: Option[Int], startIndex: Int): (Option[Int], Int) = {
        @scala.annotation.tailrec
        def add(index: Int, sum: Option[Int]): (Option[Int], Int) = {
          if (sum.nonEmpty && sum.get >= k) {
            (sum, index)
          } else if (index == nums.length) {
            (sum, index)
          } else {
            add(index + 1, Option(sum.getOrElse(0) + nums(index)))
          }
        }
        add(startIndex, currSum)
      }

      find(0, 0, None, 0)
    }
  }

}
