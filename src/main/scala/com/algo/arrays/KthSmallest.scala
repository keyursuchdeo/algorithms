package com.algo.arrays

object KthSmallest extends App {

  val array = Array.ofDim[Int](3, 3)
  array(0) = Array(1, 10, 10)
  array(1) = Array(1, 4, 5)
  array(2) = Array(2, 3, 6)

  val res = Solution.kthSmallest(array, 7)
  println(res)

  object Solution {
    def kthSmallest(mat: Array[Array[Int]], k: Int): Int = {
      val rows = mat.length

      @scala.annotation.tailrec
      def combineElements(rowIndex: Int, combinedElements: Seq[Int]): Seq[Int] = {
        if (rowIndex == rows) {
          combinedElements
        } else {
          combineElements(
            rowIndex + 1,
            combinedElements.flatMap(element => {
              mat(rowIndex).map(_ + element)
            }).sorted.take(k)
          )

        }
      }

      val elements = combineElements(1, mat(0))
      println(elements)
      elements(k - 1)
    }
  }

}
