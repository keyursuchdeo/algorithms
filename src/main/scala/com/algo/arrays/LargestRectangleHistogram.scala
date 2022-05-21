package com.algo.arrays

object LargestRectangleHistogram extends App {
  object Solution {
    def largestRectangleArea(heights: Array[Int]): Int = {

      @scala.annotation.tailrec
      def find(index: Int, prevPos: Int, posStack: Seq[Int], valStack: Seq[Int], maxArea: Int): Int = {
        if(index == heights.length && posStack.isEmpty) {
          maxArea
        } else if (index == heights.length) {
          val pos = posStack.head
          val value = valStack.head
          find(index, prevPos, posStack.tail, valStack.tail, Math.max(maxArea, value * (index - pos)))
        } else {
          valStack.headOption match {
            case Some(value) if value > heights(index) =>
              val pos = posStack.head
              val value = valStack.head
              find(index, pos, posStack.tail, valStack.tail, Math.max(maxArea, value * (index - pos)))
            case _ =>
              if(prevPos == -1) {
                find(index + 1, prevPos, index +: posStack, heights(index) +: valStack, maxArea)
              } else {
                find(index + 1, -1, prevPos +: posStack, heights(index) +: valStack, maxArea)
              }
          }
        }
      }

      find(0, -1, Nil, Nil, 0)
    }
  }
}
