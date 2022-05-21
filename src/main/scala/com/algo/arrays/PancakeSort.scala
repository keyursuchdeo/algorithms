package com.algo.arrays

object PancakeSort extends App {
  object Solution {
    def pancakeSort(A: Array[Int]): List[Int] = {

      def findMaxNumIndex(array: Array[Int]): Int = {
        @scala.annotation.tailrec
        def max(index: Int, maxIndex: Int): Int = {
          if(index == array.length) {
            maxIndex
          } else {
            if(array(index) > array(maxIndex)) {
              max(index + 1, index)
            } else {
              max(index + 1, maxIndex)
            }
          }
        }
        max(0, 0)
      }

      @scala.annotation.tailrec
      def sort(array: Array[Int], kValues: Seq[Int]): Seq[Int] = {
        if(array.length <= 1) {
          kValues.reverse
        } else {
          val maxNumIndex = findMaxNumIndex(array)
          if(maxNumIndex == array.length - 1) {
            sort(array.take(array.length - 1), kValues)
          } else if (maxNumIndex == 0) {
            sort(array.reverse.take(array.length - 1), array.length +: kValues)
          } else {
            val (before, after) = array.splitAt(maxNumIndex + 1)
            val updatedArray = (before.reverse ++ after).reverse
            sort(updatedArray.take(array.length - 1), array.length +: ((maxNumIndex + 1) +: kValues))
          }
        }
      }

      sort(A, Nil).toList
    }
  }
}
