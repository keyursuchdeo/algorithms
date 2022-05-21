package com.algo.arrays

object ThreeSumWithMultiplicity extends App {

  val input = Array(2, 2, 4, 4)
  Solution.threeSumMulti(input, 8)

  object Solution {
    def threeSumMulti(arr: Array[Int], target: Int): Int = {
      @scala.annotation.tailrec
      def prepFreqMap(index: Int, map: Map[Int, Int]): Map[Int, Int] = {
        if (index == arr.length) {
          map
        } else {
          map.get(arr(index)) match {
            case Some(freq) =>
              prepFreqMap(index + 1, map + (arr(index) -> (freq + 1)))
            case _ =>
              prepFreqMap(index + 1, map + (arr(index) -> 1))
          }
        }
      }

      @scala.annotation.tailrec
      def countThreeSum(index: Int, count: Int, freqMap: Map[Int, Int]): Int = {
        if (index == arr.length) {
          count
        } else {
          val updatedMap =
            freqMap.get(arr(index)) match {
              case Some(freq) if freq == 1 =>
                freqMap - arr(index)
              case Some(freq) =>
                freqMap + (arr(index) -> (freq - 1))
              case _ =>
                freqMap
            }
          println(updatedMap)
          val addlCount = countTwoSumFrom(index + 1, target - arr(index), updatedMap)
          println(s"${arr(index)} -> $addlCount")
          countThreeSum(index + 1, count + addlCount, updatedMap)

        }
      }

      def countTwoSumFrom(fromIndex: Int, newTarget: Int, freqMap: Map[Int, Int]) = {
        @scala.annotation.tailrec
        def countTwoSum(index: Int, count: Int): Int = {
          if (index == arr.length) {
            count
          } else {
            val diff = newTarget - arr(index)
            freqMap.get(diff) match {
              case Some(freq) if diff == arr(index) && diff == arr(fromIndex - 1) =>
                if (freq > 2) {
                  countTwoSum(index + 1, count + freq - 2)
                } else {
                  countTwoSum(index + 1, count)
                }
              case Some(freq) if diff == arr(index) || diff == arr(fromIndex - 1) =>
                if (freq > 1) {
                  countTwoSum(index + 1, count + freq - 1)
                } else {
                  countTwoSum(index + 1, count)
                }
              case Some(freq) =>
                countTwoSum(index + 1, count + freq)
              case _ =>
                countTwoSum(index + 1, count)
            }
          }
        }

        countTwoSum(fromIndex, 0)
      }

      val map = prepFreqMap(0, Map())
      countThreeSum(0, 0, map)
    }
  }

}
