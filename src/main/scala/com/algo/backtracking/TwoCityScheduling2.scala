package com.algo.backtracking

object TwoCityScheduling2 extends App {
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
      case class CurrStatus(index: Int, bucketASize: Int, bucketBSize: Int, cost: Int)

      @scala.annotation.tailrec
      def calculateCost(index: Int,
                        stack: Seq[CurrStatus],
                        currBucketASize: Int,
                        currBucketBSize: Int,
                        currMinCost: Int,
                        currCost: Int): Int = {
        if(index == numOfPeople && stack.isEmpty) {
          currMinCost
        } else if (index == numOfPeople ) {
          val poppedElement = stack.head
          if(currCost > currMinCost) {
            calculateCost(
              poppedElement.index + 1,
              stack.tail,
              poppedElement.bucketASize,
              poppedElement.bucketBSize + 1,
              currMinCost,
              poppedElement.cost + costs(poppedElement.index)(1)
            )
          } else {
            calculateCost(
              poppedElement.index + 1,
              stack.tail,
              poppedElement.bucketASize,
              poppedElement.bucketBSize + 1,
              currCost,
              poppedElement.cost + costs(poppedElement.index)(1)
            )
          }
        } else {
          if(currCost > currMinCost) {
            if(stack.nonEmpty) {
              val poppedElement = stack.head
              calculateCost(
                poppedElement.index + 1,
                stack.tail,
                poppedElement.bucketASize,
                poppedElement.bucketBSize + 1,
                currMinCost,
                poppedElement.cost + costs(poppedElement.index)(1)
              )
            } else {
              currMinCost
            }
          } else {
            if (currBucketASize == halfOfNumOfPeople) {
              calculateCost(
                index + 1,
                stack,
                currBucketASize,
                currBucketBSize + 1,
                currMinCost,
                currCost + costs(index)(1)
              )
            } else if (currBucketBSize == halfOfNumOfPeople) {
              calculateCost(
                index + 1,
                stack,
                currBucketASize + 1,
                currBucketBSize,
                currMinCost,
                currCost + costs(index)(0)
              )
            } else {
              val elementToPush = CurrStatus(index, currBucketASize, currBucketBSize, currCost)
              calculateCost(
                index + 1,
                elementToPush +: stack,
                currBucketASize + 1,
                currBucketBSize,
                currMinCost,
                currCost + costs(index)(0)
              )
            }
          }
        }
      }
      calculateCost(0, Nil, 0, 0, Int.MaxValue, 0)
    }
  }
}
