package com.algo.dp

import scala.collection.immutable

object Triangle extends App {

  object Solution {
    def minimumTotal(triangle: List[List[Int]]): Int = {
      val size = triangle.size
      val tArray = triangle.toArray
      val totalElements: Int = (size * (size + 1)) / 2
      val total = new Array[Int](totalElements)

      @scala.annotation.tailrec
      def calculateMin(level: Int): Unit = {
        if (level == size) {
          val startIndex = levelStartIndex(level)
          (0 until level).foreach(col => {
            total(startIndex + col) = tArray(level - 1)(col)
          })
          calculateMin(level - 1)
        } else if (level == 0) {
          ()
        } else {
          val startIndex = levelStartIndex(level)
          (0 until level).foreach(col => {
            total(startIndex + col) =
              tArray(level - 1)(col) +
                Math.min(
                  total(startIndex + level + col), total(startIndex + level + col + 1)
                )
          })
          calculateMin(level - 1)
        }
      }

      def levelStartIndex(level: Int): Int = {
        ((level - 1) * level) / 2
      }

      calculateMin(size)
      println(total.mkString(","))
      total(0)
    }
  }

}
