package com.algo.backtracking

object NQueensII extends App {
  object Solution {
    def totalNQueens(n: Int): Int = {
      def placeQueen(queenIndex: Int,
                     unavailableCols: Set[Int],
                     unavailableCells: Set[(Int, Int)]): Seq[((Int, Int), Set[(Int, Int)], Set[Int])] = {
        @scala.annotation.tailrec
        def place(colIndex: Int, possiblePositions: Seq[(Int, Int)]): Seq[(Int, Int)] = {
          if (colIndex == n) {
            possiblePositions
          } else {
            if (!unavailableCols.contains(colIndex) &&
              !unavailableCells.contains((queenIndex, colIndex))) {
              place(colIndex + 1, (queenIndex, colIndex) +: possiblePositions)
            } else {
              place(colIndex + 1, possiblePositions)
            }
          }
        }

        def findUnavailableCells(cell: (Int, Int)): Seq[(Int, Int)] = {
          val (row, col) = cell
          val forwardOutput: Seq[(Int, Int)] =
            (col + 1 until n).map(blockedCellCol => {
              (row + (blockedCellCol - col), blockedCellCol)
            })

          val backwardOutput: Seq[(Int, Int)] =
            (col - 1 to 0 by -1).map(blockedCellCol => {
              (row + (col - blockedCellCol), blockedCellCol)
            })

          forwardOutput ++ backwardOutput
        }

        val positions = place(0, Nil)
        val unavailabilityByPos: Seq[((Int, Int), Set[(Int, Int)], Set[Int])] =
          positions.map(position => {
            val (_, col) = position
            val updatedUnavailableCells: Set[(Int, Int)] = unavailableCells ++ findUnavailableCells(position)
            val updatedUnavailableCols: Set[Int] = unavailableCols + col
            (position, updatedUnavailableCells, updatedUnavailableCols)
          })
        unavailabilityByPos
      }

      def placeQueens(queenIndex: Int,
                      unavailableCols: Set[Int],
                      unavailableCells: Set[(Int, Int)]): Int = {
        if (queenIndex == n - 1) {
          val unavailabilityByPos: Seq[((Int, Int), Set[(Int, Int)], Set[Int])] =
            placeQueen(queenIndex, unavailableCols, unavailableCells)
          if(unavailabilityByPos.isEmpty) 0 else 1
        } else {
          val unavailabilityByPos: Seq[((Int, Int), Set[(Int, Int)], Set[Int])] =
            placeQueen(queenIndex, unavailableCols,unavailableCells)
          if (unavailabilityByPos.isEmpty) {
            0
          } else {
            unavailabilityByPos.map(unavailability => {
              val (pos, cells, cols) = unavailability
              placeQueens(queenIndex + 1, cols ++ unavailableCols, cells ++ unavailableCells)
            }).sum
          }
        }
      }

      placeQueens(0, Set(), Set())
    }
  }
}
