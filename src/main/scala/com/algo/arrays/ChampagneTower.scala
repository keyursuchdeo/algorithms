package com.algo.arrays

object ChampagneTower extends App {

  object Solution {
    def champagneTower(poured: Int, query_row: Int, query_glass: Int): Double = {
      val quantities: Array[Array[Double]] = Array.ofDim[Double](102, 102)

      @scala.annotation.tailrec
      def fillFlowThrough(row: Int, col: Int): Unit = {
        if (row == query_row && col == query_row) {
          ()
        } else if (col > query_row) {
          fillFlowThrough(row + 1, 0)
        } else if (row == 0 && col == 0) {
          quantities(row)(col) = poured
          fillFlowThrough(row, col + 1)
        } else {
          val overFlowQty = (quantities(row)(col) - 1) / 2.0
          quantities(row + 1)(col) = quantities(row + 1)(col) + overFlowQty
          quantities(row + 1)(col + 1) = quantities(row + 1)(col + 1) + overFlowQty
          fillFlowThrough(row, col + 1)
        }
      }

      fillFlowThrough(0, 0)
      Math.min(1, quantities(query_row)(query_glass))
    }
  }

}
