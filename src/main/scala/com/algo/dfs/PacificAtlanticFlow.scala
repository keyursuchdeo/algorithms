package com.algo.dfs

object PacificAtlanticFlow extends App {

  val m = Array.ofDim[Int](5, 5)
  m(0) = Array(1, 2, 2, 3, 5)
  m(1) = Array(3, 2, 3, 4, 4)
  m(2) = Array(2, 4, 5, 3, 1)
  m(3) = Array(6, 7, 1, 4, 5)
  m(4) = Array(5, 1, 1, 2, 4)

  val res = Solution.pacificAtlantic(m)
  println(res)

  object Solution {
    def pacificAtlantic(matrix: Array[Array[Int]]): List[List[Int]] = {
      val rows = matrix.length
      val cols = matrix.headOption.map(_.length).getOrElse(0)
      val visitedCells = Array.fill[String](rows, cols)("N")

      def notYetVisited(row: Int, col: Int) = visitedCells(row)(col) == "N"

      def notYetVisitedByAtlantic(row: Int, col: Int) = visitedCells(row)(col) == "N" || visitedCells(row)(col) == "P"

      def flowFromLeft(): Unit = {
        (0 until rows).foreach(row => {
          if(notYetVisited(row, 0)) dfsFromPacific(row, 0)
        })
      }

      def flowFromTop(): Unit = {
        (0 until cols).foreach(col => {
          if(notYetVisited(0, col)) dfsFromPacific(0, col)
        })
      }

      def flowFromRight(): Unit = {
        (0 until rows).foreach(row => {
          if(notYetVisitedByAtlantic(row, cols - 1)) dfsFromAtlantic(row, cols - 1)
        })
      }

      def flowFromBottom(): Unit = {
        (0 until cols).foreach(col => {
          if(notYetVisitedByAtlantic(rows - 1, col)) dfsFromAtlantic(rows - 1, col)
        })
      }

      def dfsFromPacific(row: Int, col: Int): Unit = {
        if(!notYetVisited(row, col)) {
          ()
        } else {
          visitedCells(row)(col) = "P"
          neighboursOf(row, col).collect{
            case (nRow, nCol)
              if nRow >= 0 && nRow < rows && nCol >= 0 && nCol < cols && matrix(nRow)(nCol) > matrix(row)(col) =>
              dfsFromPacific(nRow, nCol)
          }
        }
      }

      def dfsFromAtlantic(row: Int, col: Int): Unit = {
        if(!notYetVisitedByAtlantic(row, col)) {
          ()
        } else {
          visitedCells(row)(col) match {
            case "P" => visitedCells(row)(col) = "PA"
            case _ => visitedCells(row)(col) = "A"
          }
          neighboursOf(row, col).collect{
            case (nRow, nCol)
              if nRow >= 0 && nRow < rows && nCol >= 0 && nCol < cols && matrix(nRow)(nCol) > matrix(row)(col) =>
              dfsFromAtlantic(nRow, nCol)
          }
        }
      }

      def neighboursOf(row: Int, col: Int) = {
        Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
      }

      @scala.annotation.tailrec
      def paCoordinates(row: Int, col: Int, coordinates: List[List[Int]]): List[List[Int]] = {
        if (row == rows) {
          coordinates
        } else if (col == cols) {
          paCoordinates(row + 1, 0, coordinates)
        } else {
          if(visitedCells(row)(col) == "PA") {
            paCoordinates(row, col + 1, List(row, col) +: coordinates)
          } else {
            paCoordinates(row, col + 1, coordinates)
          }
        }
      }

      flowFromLeft()
      flowFromTop()
      flowFromRight()
      flowFromBottom()

      println(visitedCells.map(_.mkString(",")).mkString("|"))

      paCoordinates(0, 0, Nil)

    }
  }
}
