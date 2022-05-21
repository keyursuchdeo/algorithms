package com.algo.dp

object CherryPickup2 extends App {

//  val g = Array(Array(0,1,-1),Array(1,0,-1),Array(1,1,1))
//  val g = Array(Array(1, 1, 1), Array(1, 1, 1), Array(1, 1, 1))
  val g = Array(Array(1,1,1,1,0,0,0),Array(0,0,0,1,0,0,0),Array(0,0,0,1,0,0,1),Array(1,0,0,1,0,0,0),Array(0,0,0,1,0,0,0),Array(0,0,0,1,0,0,0),Array(0,0,0,1,1,1,1))
  val res = Solution.cherryPickup(g)
  println(res)

  object Solution {
    def cherryPickup(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)

      val output = Array.ofDim[Int](rows, cols)

      @scala.annotation.tailrec
      def resetCherries(path: Seq[(Int, Int)]): Unit = {
        if(path.isEmpty) {
          ()
        } else {
          val (row, col) = path.head
          grid(row)(col) = 0
          resetCherries(path.tail)
        }
      }

      def findMaxCherryPathFrom(row: Int, col: Int): (Option[Int], Seq[(Int, Int)]) = {
        if(row == rows - 1 && col == cols - 1) {
          (Option(grid(row)(col)), Seq((row, col)))
        } else if (row == rows - 1) {
          if(grid(row)(col) == -1) {
            (None, Nil)
          } else {
            val (cherries, path) = findMaxCherryPathFrom(row, col + 1)
            (cherries.map(_ +  grid(row)(col)), (row, col) +: path)
          }
        } else if (col == cols - 1) {
          if(grid(row)(col) == -1) {
            (None, Nil)
          } else {
            val (cherries, path) = findMaxCherryPathFrom(row + 1, col)
            (cherries.map(_ + grid(row)(col)), (row, col) +: path)
          }
        } else {
          if(grid(row)(col) == -1) {
            (None, Nil)
          } else {
            val (cherries1, path1) = findMaxCherryPathFrom(row + 1, col)
            val (cherries2, path2) = findMaxCherryPathFrom(row, col + 1)
            (cherries1, cherries2) match {
              case (Some(v1), Some(v2)) =>
                if(v1 > v2) {
                  (Some(grid(row)(col) + v1), (row, col) +: path1)
                } else {
                  (Some(grid(row)(col) + v2), (row, col) +: path2)
                }
              case (Some(v1), _) =>
                (Some(grid(row)(col) + v1), (row, col) +: path1)
              case (_, Some(v2)) =>
                (Some(grid(row)(col) + v2), (row, col) +: path2)
              case (_, _) =>
                (None, Nil)
            }
          }
        }
      }

      val (cherriesPicked1, path1) = findMaxCherryPathFrom(0, 0)
      if(path1.isEmpty) {
        0
      } else {
        resetCherries(path1)
        val (cherriesPicked2, _) = findMaxCherryPathFrom(0, 0)
        cherriesPicked1.getOrElse(0) + cherriesPicked2.getOrElse(0)
      }
    }
  }
}
