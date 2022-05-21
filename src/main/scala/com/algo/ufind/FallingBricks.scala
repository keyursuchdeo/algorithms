package com.algo.ufind

import scala.collection.mutable

object FallingBricks extends App {

//  val grid = Array.ofDim[Int](2, 4)
//  grid(0) = Array(1, 0, 0, 0)
//  grid(1) = Array(1, 1, 0, 0)
//
//  val hits = Array.ofDim[Int](2, 2)
//  hits(0) = Array(1, 1)
//  hits(1) = Array(1, 0)

//  val grid = Array.ofDim[Int](2, 4)
//  grid(0) = Array(1, 0, 0, 0)
//  grid(1) = Array(1, 1, 1, 0)
//
//  val hits = Array.ofDim[Int](1, 2)
//  hits(0) = Array(1, 0)

  val grid = Array.ofDim[Int](5, 1)
  grid(0) = Array(1)
  grid(1) = Array(1)
  grid(2) = Array(1)
  grid(3) = Array(1)
  grid(4) = Array(1)

  val hits = Array.ofDim[Int](5, 2)
  hits(0) = Array(3, 0)
  hits(1) = Array(4, 0)
  hits(2) = Array(1, 0)
  hits(3) = Array(2, 0)
  hits(4) = Array(0, 0)

  Solution.hitBricks(grid, hits)

  object Solution {
    def hitBricks(grid: Array[Array[Int]], hits: Array[Array[Int]]): Array[Int] = {

      def prepHitsCells(): Array[Cell] = {
        hits.map(hit => {
          Cell(hit(0), hit(1))
        })
      }

      case class Cell(row: Int, col: Int)
      val hitsCells: Array[Cell] = prepHitsCells()
      val allFallenBricks = new Array[Int](hits.length)
      val rowsInGrid = grid.length
      val colsInGrid = grid(0).length
      val fallenBricksQueue: mutable.Queue[Cell] = new mutable.Queue[Cell]

      @scala.annotation.tailrec
      def processHits(index: Int): Unit = {
        if(index == hits.length) {
          ()
        } else {
          val fallenBricks = processHit(hits(index)(0), hits(index)(1))
          allFallenBricks(index) = (fallenBricks.toSet -- hitsCells).size
          processHits(index + 1)
        }
      }

      def processHit(row: Int, col: Int): Seq[Cell] = {
        grid(row)(col) = 0
        fallenBricksQueue.enqueue(Cell(row, col))
        cascadeEffectOfDroppedBrick(Nil)
      }

      @scala.annotation.tailrec
      def cascadeEffectOfDroppedBrick(fallenBricks: Seq[Cell]): Seq[Cell] = {
        if(fallenBricksQueue.isEmpty) {
          fallenBricks
        } else {
          val fallenBrick = fallenBricksQueue.dequeue()
          cascadeEffectOfDroppedBrick(fallenBricks ++ accAndResetDroppedBricksInGrid(fallenBrick))
        }
      }

      def accAndResetDroppedBricksInGrid(hitCell: Cell): Seq[Cell] = {
        val droppedBricks: Seq[Cell] = accDroppedBricks(hitCell.row, hitCell.col)
        droppedBricks.foreach(cell => {
          grid(cell.row)(cell.col) = 0
        })
        //fallenBricksQueue.enqueue(droppedBricks: _*)
        droppedBricks.foreach(fallenBricksQueue.enqueue(_))
        droppedBricks
      }

      def accDroppedBricks(hitRow: Int, hitCol: Int): Seq[Cell] = {
        adjCells(hitRow, hitCol).filter(cell => {
          !isTopRow(cell) && ifBrickPresent(cell)
        })
      }

      def adjCells(row: Int, col: Int): Seq[Cell] = {
        adjCellsInRow(row, col) ++ adjCellsInCol(row, col)
      }

      def adjCellsInRow(row: Int, col: Int): Seq[Cell] = {
        val a: Option[Cell] = if (col + 1 < colsInGrid) Option(Cell(row, col + 1)) else None
        val b: Option[Cell] = if (col - 1 >= 0) Option(Cell(row, col - 1)) else None
        Seq(a, b).flatten
      }

      def adjCellsInCol(row: Int, col: Int): Seq[Cell] = {
        val a: Option[Cell] = if (row + 1 < rowsInGrid) Option(Cell(row + 1, col)) else None
        val b: Option[Cell] = if (row - 1 >= 0) Option(Cell(row - 1, col)) else None
        Seq(a, b).flatten
      }

      def ifBrickPresent(cell: Cell): Boolean = grid(cell.row)(cell.col) == 1

      def isTopRow(cell: Cell): Boolean = cell.row == 0

      processHits(0)
      println(allFallenBricks.mkString(","))
      allFallenBricks
    }
  }
}
