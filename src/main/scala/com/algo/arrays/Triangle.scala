package com.algo.arrays

object Triangle extends App {
  object Solution {
    def minimumTotal(triangle: List[List[Int]]): Int = {
      val triangleArr: Array[Array[Int]] = triangle.map(_.toArray).toArray
      var cache: Map[(Int, Int), Int] = Map[(Int, Int), Int]()
      def find(index: Int, row: Int): Int= {
        if(row == triangleArr.length) {
          0
        } else {
          cache.get((index, row)) match {
            case Some(value) => value
            case _ =>
              val value =
                Math.min(
                  triangleArr(row)(index) + find(index, row + 1),
                  triangleArr(row)(index) + find(index + 1, row + 1)
                )
              cache = cache + ((index, row) -> value)
              value
          }
        }
      }
      find(0, 0)
    }
  }
}
