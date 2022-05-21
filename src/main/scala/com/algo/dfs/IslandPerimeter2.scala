package com.algo.dfs

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object IslandPerimeter2 extends App {

  val g = Array.ofDim[Int](4, 4)
  g(0) = Array(0, 1, 0 , 0)
  g(1) = Array(1, 1, 1, 0)
  g(2) = Array(0, 1, 0, 0)
  g(3) = Array(1, 1, 0, 0)

//  val g = Array.ofDim[Int](1, 4)
//  g(0) = Array(0, 0, 0 , 0)
  val res = Solution.islandPerimeter(g)
  println(res)

  object Solution {
    def islandPerimeter(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)

      case class Cell(row: Int, col: Int) {
        def neighbouringCell(rowDelta: Int, colDelta: Int): Cell = {
          Cell(row + rowDelta, col + colDelta)
        }
      }
      lazy val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))

      @scala.annotation.tailrec
      def calculatePerimeter(row: Int, col: Int, currP: Int): Int = {
        if(row == rows) {
          currP
        } else if (col == cols) {
          calculatePerimeter(row + 1, 0, currP)
        } else {
          if(grid(row)(col) == 0) {
            calculatePerimeter(row, col + 1, currP)
          } else {
            calculatePerimeter(row, col + 1, currP + 4 - neighbouringIslandCells(Cell(row, col)).length)
          }
        }
      }

      def neighbouringIslandCells(cell: Cell): Array[Cell] = {
        dirs.map(dir => {
          val (rowD, colD) = dir
          cell.neighbouringCell(rowD, colD)
        }).filter(nc => {
          nc.row >= 0 &&
            nc.row < rows &&
            nc.col >= 0 &&
            nc.col < cols &&
            grid(nc.row)(nc.col) == 1
        })
      }

      calculatePerimeter(0, 0, 0)
    }
  }

}
