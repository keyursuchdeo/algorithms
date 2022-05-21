package com.algo.dp

import scala.collection.mutable

object UniquePath extends App {
//  val m = 3
//  val n = 2
  val m = 7
  val n = 3
  val res = uniquePaths(m, n)
  println(res)

  def uniquePaths(m: Int, n: Int): Int = {
    var map = mutable.Map[(Int, Int), Int]()
    def findPaths(col: Int, row: Int): Int = {
      if(col == m && row == n) {
        getFromMapOrMemoize(col, row, 1)
      } else if (col == m) {
        getFromMapOrMemoize(col, row, findPaths(col, row + 1))
      } else if (row == n) {
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

    findPaths(1, 1)
  }
}

//object Solution1 {
//  def uniquePaths1(m: Int, n: Int): Int = {
//    def findPaths(col: Int, row: Int): Seq[(Int, Int)] = {
//      if(col == m && row == n) {
//        Seq((m, n))
//      } else {
//        ((col, row) +: findPaths(col + 1, row)) ++ ((col, row) +: findPaths(col, row + 1))
//      }
//    }
//
//    val paths = findPaths(0, 0)
//    paths.size
//  }
//}
