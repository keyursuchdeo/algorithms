package com.algo.arrays

object MinDominoRotation extends App {
  object Solution {
    def minDominoRotations(A: Array[Int], B: Array[Int]): Int = {
      
      @scala.annotation.tailrec
      def recordNumPositionsAndFrequency(index: Int, array: Array[Int], output: Array[(Set[Int], Int)]): Array[(Set[Int], Int)] = {
        if(index == array.length) {
          output
        } else {
          val (positions, frequency) = output(array(index))
          output(array(index)) = (positions + index, frequency + 1)
          recordNumPositionsAndFrequency(index + 1, array, output)
        }
      }

      @scala.annotation.tailrec
      def findMin(numPositionsA: Array[(Set[Int], Int)], numPositionsB: Array[(Set[Int], Int)], currNum: Int = 1, currMin: Int): Int = {
        if(currNum == numPositionsA.length) {
          currMin
        } else {
          val (currNumPositions, currNumFrequency) = numPositionsA(currNum)
          if(currNumFrequency == A.length) {
            0
          } else {
            val requiredSwaps = A.length - currNumFrequency
            if(requiredSwaps < currMin) {
              val (availablePositions, swapsAvailable) = numPositionsB(currNum)
              if(swapsAvailable >= requiredSwaps) {
                if((availablePositions -- currNumPositions).size == requiredSwaps) {
                  findMin(numPositionsA, numPositionsB, currNum + 1, requiredSwaps)
                } else {
                  findMin(numPositionsA, numPositionsB, currNum + 1, currMin)
                }
              } else {
                findMin(numPositionsA, numPositionsB, currNum + 1, currMin)
              }
            } else {
              findMin(numPositionsA, numPositionsB, currNum + 1, currMin)
            }
          }
        }
      }

      val aNumFrequency = recordNumPositionsAndFrequency(0, A, Array.fill[(Set[Int], Int)](7)((Set(), 0)))
      val bNumFrequency = recordNumPositionsAndFrequency(0, B, Array.fill[(Set[Int], Int)](7)((Set(), 0)))

      val min1 = findMin(aNumFrequency, bNumFrequency, 1, Int.MaxValue)
      if(min1 == 0) {
        0
      } else {
        val min2 = findMin(bNumFrequency, aNumFrequency, 1, Int.MaxValue)
        if(min1 == Int.MaxValue && min2 == Int.MaxValue) {
          -1
        } else {
          Math.min(min1, min2)
        }
      }
    }
  }
}
