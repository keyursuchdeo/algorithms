package com.algo.dfs

import scala.collection.mutable

object IslandPerimeter extends App {

//  val g = Array.ofDim[Int](4, 4)
//  g(0) = Array(0, 1, 0 , 0)
//  g(1) = Array(1, 1, 1, 0)
//  g(2) = Array(0, 1, 0, 0)
//  g(3) = Array(1, 1, 0, 0)

  val g = Array.ofDim[Int](1, 4)
  g(0) = Array(0, 0, 0 , 0)
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
      def findStartingCell(row: Int, col: Int): Option[Cell] = {
        if (row == rows) {
          None
        } else if (col == cols) {
          findStartingCell(row + 1, 0)
        } else {
          if (grid(row)(col) == 1) {
            Option(Cell(row, col))
          } else {
            findStartingCell(row, col + 1)
          }
        }
      }


      @scala.annotation.tailrec
      def bfs(queue: mutable.Queue[Cell], currP: Int, visitedCells: Array[Array[Int]]): Int = {
        if (queue.isEmpty) {
          currP
        } else {
          val cell = queue.dequeue()
          if(visitedCells(cell.row)(cell.col) == 0) {
            val neighbouringIslands: Array[Cell] = neighbouringIslandCells(cell, visitedCells)
            neighbouringIslands.foreach(queue.enqueue(_))
            visitedCells(cell.row)(cell.col) = 1
            bfs(queue, currP + 4 - neighbouringIslands.length, visitedCells)
          } else {
            bfs(queue, currP, visitedCells)
          }

        }
      }

      def neighbouringIslandCells(cell: Cell, visitedCells: Array[Array[Int]]): Array[Cell] = {
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

      findStartingCell(0, 0) match {
        case Some(startingCell) =>
          val visitedCells: Array[Array[Int]] = Array.ofDim[Int](rows, cols)
          val queue = new mutable.Queue[Cell]()
          queue.enqueue(startingCell)
          bfs(queue, 0, visitedCells)
        case None => 0
      }
    }
  }

}
