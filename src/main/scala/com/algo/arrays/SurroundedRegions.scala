package com.algo.arrays

object SurroundedRegions extends App {

  val b = Array.ofDim[Char](4, 4)
  b(0) = Array('X', 'X', 'X', 'X')
  b(1) = Array('X', 'O', 'O', 'X')
  b(2) = Array('X', 'O', 'O', 'X')
  b(3) = Array('X', 'O', 'X', 'X')

  Solution.solve(b)
  b.foreach(row => {
    println(row.mkString(","))
  })

  object Solution {
    def solve(board: Array[Array[Char]]): Unit = {

      val rows = board.length
      val cols = board.headOption.map(_.length).getOrElse(0)

      @scala.annotation.tailrec
      def find(row: Int = 1, col: Int = 1, xColsByRow: Map[Int, (Int, Int)], xRowsByCol: Map[Int, (Int, Int)]): Unit = {
        if(row == rows - 1) {
          ()
        } else if (col == cols - 1) {
          find(row + 1, 1, xColsByRow, xRowsByCol)
        } else {
          if(board(row)(col) == 'O' && isSurrounded(row, col, xColsByRow, xRowsByCol)) {
            board(row)(col) = 'X'
            find(row, col + 1, xColsByRow, xRowsByCol)
          } else {
            find(row, col + 1, xColsByRow, xRowsByCol)
          }
        }
      }

      def isSurrounded(row: Int, col: Int, xColsByRow: Map[Int, (Int, Int)], xRowsByCol: Map[Int, (Int, Int)]): Boolean = {
        val optXCols = xColsByRow.get(row)
        val optXRows = xRowsByCol.get(col)
        (optXCols, optXRows) match {
          case (Some((lowCol, hiCol)), Some((lowRow, hiRow))) =>
            (col > lowCol && col < hiCol) && (row > lowCol && row < hiRow)
          case _ => false
        }
      }

      @scala.annotation.tailrec
      def findSurroundingXColsByRow(row: Int = 1, map: Map[Int, (Int, Int)]): Map[Int, (Int, Int)] = {
        if(row == rows - 1) {
          map
        } else {
          val surroundedXIndices: Array[(Char, Int)] = board(row).zipWithIndex.filter(_._1 == 'X')
          if(surroundedXIndices.nonEmpty) {
            val lowerIndex = surroundedXIndices.head
            val higherIndex = surroundedXIndices(surroundedXIndices.length - 1)
            findSurroundingXColsByRow(row + 1, map + (row -> (lowerIndex._2, higherIndex._2)))
          } else {
            findSurroundingXColsByRow(row + 1, map)
          }
        }
      }

      @scala.annotation.tailrec
      def findSurroundingXRowsByCol(col: Int = 1, map: Map[Int, (Int, Int)]): Map[Int, (Int, Int)] = {
        if(col == cols - 1) {
          map
        } else {
          val surroundedXIndices: Array[(Char, Int)] = colDataWithIndices(col).filter(_._1 == 'X')
          if(surroundedXIndices.nonEmpty) {
            val higherIndex = surroundedXIndices.head
            val lowerIndex = surroundedXIndices(surroundedXIndices.length - 1)
            findSurroundingXRowsByCol(col + 1, map + (col -> (lowerIndex._2, higherIndex._2)))
          } else {
            findSurroundingXRowsByCol(col + 1, map)
          }
        }
      }

      def colDataWithIndices(col: Int): Array[(Char, Int)] = {
        @scala.annotation.tailrec
        def find(row: Int, data: Seq[(Char, Int)]): Array[(Char, Int)] = {
          if(row == rows) {
            data.toArray
          } else {
            find(row + 1, (board(row)(col), row) +: data)
          }
        }

        find(0, Nil)
      }

      if (rows <= 2 || cols <= 2) {
        ()
      } else {
        val xColsByRow = findSurroundingXColsByRow(map = Map())
        val xRowsByCol = findSurroundingXRowsByCol(map = Map())
        find(1, 1, xColsByRow, xRowsByCol)
      }
    }

  }
}
