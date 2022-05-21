package com.algo.dp


object JumpGameIV extends App {

  val input = Array(100, 404, 100, 23, 23, 23, 3, 404)
//  val input = Array(7, 6, 9, 6, 9, 6, 9, 7)
  val res = Solution.minJumps(input)
  println(res)

  object Solution {
    def minJumps(arr: Array[Int]): Int = {

      @scala.annotation.tailrec
      def mapIndices(index: Int = 0, map: Map[Int, Set[Int]] = Map()): Map[Int, Set[Int]] = {
        if (index == arr.length) {
          map
        } else {
          map.get(arr(index)) match {
            case None =>
              mapIndices(index + 1, map + (arr(index) -> Set(index)))
            case Some(indices) =>
              mapIndices(index + 1, map + (arr(index) -> (indices + index)))
          }
        }
      }

      var visited = Set[Int]()

      def countJumpsFrom(fromIndex: Int, numIndices: Map[Int, Set[Int]]): Int = {
        println(visited)
        if (fromIndex == arr.length - 1) {
          0
        } else {
          val neighbours =
            if (fromIndex == 0) {
              (numIndices(arr(fromIndex)) + (fromIndex + 1) - fromIndex) -- visited
            } else {
              (numIndices(arr(fromIndex)) + (fromIndex + 1) + (fromIndex - 1) - fromIndex) -- visited
            }
          if (neighbours.isEmpty) {
            arr.length
          } else {
            visited = (visited + fromIndex) ++ neighbours
            1 +
              neighbours.map(index => {
                countJumpsFrom(index, numIndices)
              }).min
          }
        }
      }

      countJumpsFrom(0, mapIndices())
    }
  }

}
