package com.algo.dp

object CherryPickup extends App {

//  val g = Array(Array(0,1,-1),Array(1,0,-1),Array(1,1,1))
  val g = Array(Array(0,1,1),Array(1,1,1),Array(-1,1,1),Array(0,1,1,1,0),Array(1,0,-1,0,0))
  val res = Solution.cherryPickup(g)
  println(res)

  object Solution {
    def cherryPickup(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)

      def cherriesInCell(row: Int, col: Int): Option[Int] = {
        if(grid(row)(col) == -1) {
          None
        } else {
          val value = grid(row)(col)
          grid(row)(col) = 0
          Option(value)
        }
      }

      def pick(row: Int, col: Int): Option[Int] = {
        if(row == rows - 1 && col == cols - 1) {
          cherriesInCell(row, col)
        } else if (row == rows - 1) {
          cherriesInCell(row, col) match {
            case None => None
            case Some(value) =>
              pick(row, col + 1) match {
                case Some(v1) => Option(v1 + value)
                case _ =>
                  grid(row)(col) = -1
                  None
              }
          }
        } else if (col == cols - 1) {
          cherriesInCell(row, col) match {
            case None => None
            case Some(value) =>
              pick(row + 1, col) match {
                case Some(v1) => Option(v1 + value)
                case _ =>
                  grid(row)(col) = -1
                  None
              }
          }
        } else {
          cherriesInCell(row, col) match {
            case None => None
            case Some(value) =>
              (pick(row + 1, col), pick(row, col + 1)) match {
                case (Some(v1), Some(v2)) => Option(value + v1 + v2)
                case (Some(v1), _) => Option(value + v1)
                case (_, Some(v2)) => Option(value + v2)
                case _ =>
                  grid(row)(col) = -1
                  None
              }
          }
        }
      }

      pick(0, 0).getOrElse(0)
    }
  }
}
