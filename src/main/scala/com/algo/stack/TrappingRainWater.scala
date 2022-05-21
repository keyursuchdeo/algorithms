package com.algo.stack

object TrappingRainWater extends App {
  object Solution {
    def trap(height: Array[Int]): Int = {
      @scala.annotation.tailrec
      def calculateForward(index: Int, maxTillNowIndex: Int, stack: Seq[Int], output: Int): (Int, Int) = {
        if(index == height.length) {
          (output, maxTillNowIndex)
        } else {
          if(stack.isEmpty) {
            calculateForward(index + 1, index, height(index) +: stack, output)
          } else {
            if(height(index) < height(maxTillNowIndex)) {
              calculateForward(index + 1, maxTillNowIndex, height(index) +: stack, output)
            } else {
              calculateForward(index, maxTillNowIndex, stack.tail, (height(maxTillNowIndex) - stack.head) + output)
            }
          }
        }
      }

      @scala.annotation.tailrec
      def calculateBackward(index: Int, maxIndex: Int, maxTillNowIndex: Int, stack: Seq[Int], output: Int): Int = {
        if(index < maxIndex) {
          output
        } else {
          if(stack.isEmpty) {
            calculateBackward(index - 1, maxIndex, index, height(index) +: stack, output)
          } else {
            if(height(index) < height(maxTillNowIndex)) {
              calculateBackward(index - 1, maxIndex, maxTillNowIndex, height(index) +: stack, output)
            } else {
              calculateBackward(index, maxIndex, maxTillNowIndex, stack.tail, (height(maxTillNowIndex) - stack.head) + output)
            }
          }
        }
      }

      val (intermediateOutput, index) = calculateForward(0, 0, Nil, 0)
      calculateBackward(height.length - 1, index, 0, Nil, intermediateOutput)
    }
  }
}
