package com.algo.dp

import scala.collection.mutable

object UniquePath2 extends App {
//  val m = 3
//  val n = 2
  val a = Array.ofDim[Int](1, 2)
//  a(0)(1) = 1
//  a(1)(1) = 1
//  a(2)(2) = 1
  val res = uniquePathsWithObstacles(a)
  println(res)

  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    val numOfRows = obstacleGrid.length
    val numOfCols = obstacleGrid(0).length
    var map = mutable.Map[(Int, Int), Int]()
    def findPaths(col: Int, row: Int): Int = {
      if(obstacleGrid(row)(col) == 1) {
        0
      } else if(col == numOfCols - 1 && row == numOfRows - 1) {
        getFromMapOrMemoize(col, row, 1)
      } else if (col == numOfCols - 1) {
        getFromMapOrMemoize(col, row, findPaths(col, row + 1))
      } else if (row == numOfRows - 1) {
        getFromMapOrMemoize(col, row, findPaths(col + 1, row))
      } else {
        getFromMapOrMemoize(col, row, (findPaths(col + 1, row) + findPaths(col, row + 1)))
      }
    }

    def getFromMapOrMemoize(col: Int, row: Int, calcPath: => Int) = {
      map.get((col, row)) match {
        case Some(p) =>
          p
        case _ =>
          val p = calcPath
          map = map + ((col, row) -> p)
          p
      }
    }
    findPaths(0, 0)
  }
}
