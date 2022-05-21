package com.algo.dp

import scala.util.{Failure, Success, Try}

object WordSearch extends App {

  val b = Array.ofDim[Char](2, 2)
  b(0) = Array('A', 'B')
  b(1) = Array('C', 'D')

  val w = "ABC"

  val res = Solution.exist(b, w)
  println(res)

  object Solution {
    def exist(board: Array[Array[Char]], word: String): Boolean = {
      val rows = board.length
      val cols = board.headOption.map(_.length).getOrElse(0)
      val totalElements = rows * cols
      case class Cell(row: Int, col: Int) {
        private val isRowValid = row >= 0 && row <= rows - 1
        private val isColValid = col >= 0 && col <= cols - 1
        require(isRowValid && isColValid)
      }
      def find(currCell: Cell, currWord: String, visitedCells: Set[Cell], numOfVisitedCells: Int): Boolean = {
        if (currWord.isEmpty) {
          true
        } else if (numOfVisitedCells == totalElements) {
          false
        } else {
          if (board(currCell.row)(currCell.col) == currWord.head) {
            val paths = possiblePathsFrom(currCell, visitedCells)
            if(paths.isEmpty) {
              currWord.tail.isEmpty
            } else {
              paths.exists(cell => {
                find(cell, currWord.tail, visitedCells + currCell, numOfVisitedCells + 1) ||
                  find(cell, currWord, visitedCells, numOfVisitedCells)
              })
            }
          } else {
            possiblePathsFrom(currCell, visitedCells).exists(cell => {
              find(cell, currWord, visitedCells + currCell, numOfVisitedCells + 1)
            })
          }
        }
      }

      def possiblePathsFrom(cell: Cell, visitedCells: Set[Cell]): Seq[Cell] = {
        val row = cell.row
        val col = cell.col
        val prepCell: (Int, Int) => Option[Cell] = prepCellGivenVisitedCells(visitedCells)
        val possibleNextCells: Seq[Option[Cell]] =
          if (row == 0 && col == 0) {
            Seq(prepCell(row + 1, col), prepCell(row, col + 1))
          } else if (row == 0) {
            Seq(prepCell(row + 1, col), prepCell(row, col + 1), prepCell(row, col - 1))
          } else if (cell.col == 0) {
            Seq(prepCell(row + 1, col), prepCell(row, col + 1), prepCell(row - 1, col))
          } else if (row == rows - 1 && col == cols - 1) {
            Seq(prepCell(row - 1, col), prepCell(row, col - 1))
          } else if (row == rows - 1) {
            Seq(prepCell(row, col + 1), prepCell(row, col - 1), prepCell(row - 1, col))
          } else if (col == cols - 1) {
            Seq(prepCell(row - 1, col), prepCell(row + 1, col), prepCell(row, col - 1))
          } else {
            Seq(prepCell(row + 1, col), prepCell(row - 1, col), prepCell(row, col + 1), prepCell(row, col - 1))
          }
        possibleNextCells.flatten
      }

      def prepCellGivenVisitedCells(visitedCells: Set[Cell])(row: Int, col: Int): Option[Cell] = {

        Try(Cell(row, col)) match {
          case Failure(_) => None
          case Success(cell) if visitedCells.contains(cell) => None
          case Success(cell) => Some(cell)
        }
      }

      find(Cell(0, 0), word, Set(), 0) || find(Cell(0, 0), word.reverse, Set(), 0)
    }
  }

}
