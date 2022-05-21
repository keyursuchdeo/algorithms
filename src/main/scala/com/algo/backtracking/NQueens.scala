package com.algo.backtracking
import scala.collection.immutable

object NQueens extends App {

  Solution.solveNQueens(5)

  object Solution {
    def solveNQueens(n: Int): List[List[String]] = {
      val output: Seq[Array[Int]] = placeQueens(n)
//      output.foreach(arr => println(arr.mkString(",")))
      val a = printArrangements(output, n)
      println(a)
      a
    }

    private def printArrangements(output: Seq[Array[Int]], n: Int): List[List[String]] = {
      output.zipWithIndex.map(oi => {
        val (o, _) = oi
        printArrangement(o, n)
      }).toList
    }

    private def printArrangement(output: Array[Int], n: Int): List[String] = {
      val pOutput = output.map(o => {
        var sb = new scala.collection.mutable.StringBuilder()
        (0 until n).foreach(i => {
          sb.append(if(i == o) "Q" else ".")
        })
        sb.toString()
      }).toList

      pOutput
    }

    private def placeQueens(n: Int): Seq[Array[Int]] = {

      lazy val allColPos: Set[Int] = (0 until n).toSet

      @scala.annotation.tailrec
      def place(queen: Int,
                queenColPos: Array[Int],
                queenAvCols: Array[Set[Int]],
                queenUnavDiagonals: Array[Set[Cell]],
                currAvCols: Set[Int],
                currUnavDiagonals: Set[Cell],
                possibleQueenArrangements: Seq[Array[Int]]): Seq[Array[Int]] = {
        if (queen == n) {
          if(queenAvCols(0).nonEmpty) {
            place(
              0,
              Array.fill[Int](n)(-1),
              new Array[Set[Int]](n),
              new Array[Set[Cell]](n),
              queenAvCols(0),
              Set(),
              queenColPos +: possibleQueenArrangements
            )
          } else {
            queenColPos +: possibleQueenArrangements
          }
        } else {
          placeQueenInCol(queen, currAvCols, currUnavDiagonals) match {
            case Some(pos) =>
              queenColPos(queen) = pos
              queenAvCols(queen) = availableCols(pos, n, queenColPos)
              queenUnavDiagonals(queen) = currUnavDiagonals
              place(
                queen + 1,
                queenColPos,
                queenAvCols,
                queenUnavDiagonals,
                currAvailableCols(queenColPos, allColPos),
                accumulateUnavailableDiagonals(currUnavDiagonals, n, Cell(queen, pos)),
                possibleQueenArrangements)
            case None =>
              if(queen == 0) {
                possibleQueenArrangements
              } else {
                queenColPos(queen) = -1
                queenAvCols(queen) = Set()
                queenUnavDiagonals(queen) = Set()
                place(
                  queen - 1,
                  queenColPos,
                  queenAvCols,
                  queenUnavDiagonals,
                  queenAvCols(queen - 1),
                  queenUnavDiagonals(queen - 1),
                  possibleQueenArrangements)
              }
          }
        }
      }

      place(queen = 0, Array.fill[Int](n)(-1), new Array[Set[Int]](n), new Array[Set[Cell]](n), allColPos, Set(), Nil)
    }

    private def availableCols(currPosition: Int, n: Int, occupiedPositions: Array[Int]): Set[Int] =
      (currPosition + 1 until n).toSet -- occupiedPositions

    private def currAvailableCols(currQueenColPos: Array[Int], allColPos: Set[Int]): Set[Int] =
      allColPos -- currQueenColPos

    private def accumulateUnavailableDiagonals(unavDiagonals: Set[Cell], n: Int, cell: Cell): Set[Cell] = {
      unavailableDiagonalPositions(n, cell) ++ unavDiagonals
    }

    private def placeQueenInCol(queen: Int, avCols: Set[Int], unavDiagonals: Set[Cell]): Option[Int] = {
      @scala.annotation.tailrec
      def placeInCol(cols: Seq[Int]): Option[Int] = {
        cols match {
          case Nil => None
          case col :: _ if isQueenSafe(Cell(queen, col), unavDiagonals) => Option(col)
          case _ :: remainingCols => placeInCol(remainingCols)
        }
      }
      placeInCol(avCols.toList)
    }

    private def isQueenSafe(cell: Cell, diagonals: Set[Cell]): Boolean =
      !diagonals.contains(cell)

    private def unavailableDiagonalPositions(n: Int, cell: Cell): Set[Cell] =
      cell.diagonals(n)

    private case class Cell(row: Int, col: Int) {

      def diagonals(n: Int): Set[Cell] = {

        def accumulateDownwardRightDiagonalCells(): Set[Cell] =
          accumulateDiagonalCells(Set[Cell](), this.plusOne, this)

        def accumulateUpwardLeftDiagonalCells(): Set[Cell] =
          accumulateDiagonalCells(Set[Cell](), this.minusOne, this)

        def accumulateDownwardLeftDiagonalCells(): Set[Cell] =
          accumulateDiagonalCells(Set[Cell](), this.plusRowMinusColOne, this)

        def accumulateUpwardRightDiagonalCells(): Set[Cell] =
          accumulateDiagonalCells(Set[Cell](), this.minusRowPlusColOne, this)

        @scala.annotation.tailrec
        def accumulateDiagonalCells(diagonalCells: Set[Cell], getNextCell: Cell => Cell, startCell: Cell): Set[Cell] = {
          val nextCell = getNextCell(startCell)
          if(isCellInSquare(n, nextCell)) {
            accumulateDiagonalCells(diagonalCells + nextCell, getNextCell, nextCell)
          } else {
            diagonalCells
          }
        }

        accumulateDownwardRightDiagonalCells() ++
          accumulateUpwardLeftDiagonalCells() ++
          accumulateDownwardLeftDiagonalCells() ++
          accumulateUpwardRightDiagonalCells()
      }

      private def plusOne(c: Cell): Cell = Cell(c.row + 1, c.col + 1)
      private def minusOne(c: Cell): Cell = Cell(c.row - 1, c.col - 1)
      private def plusRowMinusColOne(c: Cell): Cell = Cell(c.row + 1, c.col - 1)
      private def minusRowPlusColOne(c: Cell): Cell = Cell(c.row - 1, c.col + 1)
      private def isCellInSquare(n: Int, cell: Cell): Boolean =
        (cell.row >= 0 && cell.row < n) && (cell.col >= 0 && cell.col < n)
    }
  }
}
