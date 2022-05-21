package com.algo.dp

object TwoCityScheduling extends App {
  val input = Array.ofDim[Int](6, 2)
  input(0) = Array(259, 770)
  input(1) = Array(448, 54)
  input(2) = Array(926, 667)
  input(3) = Array(184, 139)
  input(4) = Array(840, 118)
  input(5) = Array(577, 469)

  val res = Solution.twoCitySchedCost(input)
  println(res)

  object Solution {
    def twoCitySchedCost(costs: Array[Array[Int]]): Int = {
      val numOfPeople = costs.length
      val halfOfNumOfPeople = numOfPeople / 2
      def calculateCost(index: Int, currCost: Int, currBucketASize: Int, currBucketBSize: Int): Int = {
        println(currCost)
        if(index == numOfPeople) {
          currCost
        } else if (currBucketASize == halfOfNumOfPeople) {
          calculateCost(index + 1, costs(index)(1) + currCost, currBucketASize, currBucketBSize + 1)
        } else if (currBucketBSize == halfOfNumOfPeople) {
          costs(index)(0) + calculateCost(index + 1, costs(index)(0) + currCost, currBucketASize + 1, currBucketBSize)
        } else {
          Math.min(
            calculateCost(index + 1, costs(index)(0) + currCost, currBucketASize + 1, currBucketBSize),
            calculateCost(index + 1, costs(index)(1) + currCost, currBucketASize, currBucketBSize + 1),
          )
        }
      }
      calculateCost(0, 0, 0, 0)
    }
  }
}
