package com.algo.dp

object BurstBalloonsAgain extends App {
  object Solution {
    def maxCoins(nums: Array[Int]): Int = {

      var map: Map[Seq[Int], Int] = Map[Seq[Int], Int]()

      def calculate(index: Int, currNums: Seq[Int]): Int = {
        if(index == 0) {
          currNums(index) * currNums(index + 1)
        } else if (index == currNums.length - 1) {
          currNums(index) * currNums(index - 1)
        } else {
          currNums(index) * currNums(index + 1) * currNums(index - 1)
        }
      }

      def calculateMax(currNums: Seq[Int]): Int = {
        if(currNums.isEmpty) {
          0
        } else if (currNums.length == 1) {
          currNums.head
        } else {
          map.get(currNums) match {
            case Some(value) => value
            case _ =>
              val value =
                currNums.indices.map(index => {
                  val (before, after) = currNums.splitAt(index)
                  calculate(index, currNums) + calculateMax(before ++ after.tail)
                }).max
              map = map + (currNums -> value)
              value

          }
        }
      }

      calculateMax(nums.toSeq)
    }
  }
}
