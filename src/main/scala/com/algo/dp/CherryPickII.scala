package com.algo.dp

object CherryPickII extends App {

  val a = Array(Array(3,1,1),Array(2,5,1),Array(1,5,5),Array(2,1,1))
  val res = Solution.cherryPickup(a)
  println(res)

  object Solution {
    def cherryPickup(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)

      val dp = Array.fill[Int](rows, cols, cols)(-1)

      def findNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        if (row == rows - 1) {
          Nil
        } else {
          Seq((row + 1, col - 1), (row + 1, col), (row + 1, col + 1)).filter(cell => {
            val (_, cc) = cell
            cc >= 0 && cc < cols
          })
        }
      }

      def invalidCol(col: Int): Boolean = col < 0 || col >= cols

      def findFrom(row: Int, r1Col: Int, r2Col: Int): Int = {
        if (invalidCol(r1Col) || invalidCol(r2Col)) {
          0
        } else if (dp(row)(r1Col)(r2Col) != -1) {
          dp(row)(r1Col)(r2Col)
        } else {
          val tempResult =
            if (r1Col == r2Col) {
              grid(row)(r1Col)
            } else {
              grid(row)(r1Col) + grid(row)(r2Col)
            }

          val result =
            if (row == rows - 1) {
              tempResult
            } else {
              tempResult +
                (r1Col - 1 to r1Col + 1).flatMap(newR1Col => {
                  (r2Col - 1 to r2Col + 1).map(newR2Col => {
                    findFrom(row + 1, newR1Col, newR2Col)
                  })
                }).max
            }
          dp(row)(r1Col)(r2Col) = result
          result
        }
      }

      val a = findFrom(0, 0, cols - 1)
      println(dp.map(_.map(_.mkString(",")).mkString("_")).mkString("|"))
      a
    }
  }

}
