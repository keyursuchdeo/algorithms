package com.algo.arrays

object BrickWall extends App {
  object Solution {
    def leastBricks(wall: List[List[Int]]): Int = {
      var freqMap: Map[Int, Int] = Map[Int, Int]()

      @scala.annotation.tailrec
      def fillFreqMap(wallRows: List[List[Int]]): Unit = {
        if(wallRows.isEmpty) {
          ()
        } else {
          processRow(wallRows.head, 0)
          fillFreqMap(wallRows.tail)
        }
      }

      @scala.annotation.tailrec
      def processRow(row: List[Int], rowSum: Int): Unit = {
        if(row.isEmpty) {
          ()
        } else {
          freqMap = freqMap + (rowSum + row.head -> (freqMap.getOrElse(rowSum + row.head, 0) + 1))
          processRow(row.tail, rowSum + row.head)
        }
      }

      fillFreqMap(wall)
      wall.size - freqMap.values.max
    }
  }
}
