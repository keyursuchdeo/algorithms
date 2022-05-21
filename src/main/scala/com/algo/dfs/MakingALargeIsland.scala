package com.algo.dfs

object MakingALargeIsland extends App {

  object Solution {
    def largestIsland(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.headOption.map(_.length).getOrElse(0)
      val currIslandSizes: Array[Array[Int]] = Array.fill[Int](rows, cols)(-1)
      val cellGroups = Array.fill[Int](rows, cols)(-1)

      @scala.annotation.tailrec
      def flipZeros(row: Int, col: Int, currMax: Int, groupMap: Map[Int, Int], zeroFound: Boolean): Int = {
        if (row == rows) {
          if (!zeroFound) rows * cols else currMax
        } else if (col == cols) {
          flipZeros(row + 1, 0, currMax, groupMap, zeroFound)
        } else {
          if (grid(row)(col) != 0) {
            flipZeros(row, col + 1, currMax, groupMap, zeroFound)
          } else {
            val neighbours = findNeighbours(row, col)
            val neighbourUniqueGroups =
              neighbours.map(cell => {
                val (nRow, nCol) = cell
                cellGroups(nRow)(nCol)
              }).toSet
            val size =
              1 +
                neighbourUniqueGroups.toSeq.map(group => {
                  groupMap(group)
                }).sum
            flipZeros(row, col + 1, Math.max(currMax, size), groupMap, zeroFound = true)
          }
        }
      }

      @scala.annotation.tailrec
      def fillCurrIslandSizes(row: Int, col: Int, groupMap: Map[Int, Int], currGroup: Int): Map[Int, Int] = {
        if (row == rows) {
          groupMap
        } else if (col == cols) {
          fillCurrIslandSizes(row + 1, 0, groupMap, currGroup)
        } else {
          if (grid(row)(col) == 0 || currIslandSizes(row)(col) != -1) {
            currIslandSizes(row)(col) = 0
            fillCurrIslandSizes(row, col + 1, groupMap, currGroup)
          } else {
            dfsFrom(row, col, currGroup)
            fillCurrIslandSizes(row, col + 1, groupMap + (currGroup -> currIslandSizes(row)(col)), currGroup + 1)
          }
        }
      }

      def isValid(cell: (Int, Int)): Boolean = {
        val (row, col) = cell
        row < rows && row >= 0 && col < cols && col >= 0
      }

      def findNeighbours(row: Int, col: Int): Seq[(Int, Int)] = {
        Seq((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)).filter(isValid)
      }

      def dfsFrom(row: Int, col: Int, group: Int): Int = {
        cellGroups(row)(col) = group
        if (currIslandSizes(row)(col) == -1) {
          currIslandSizes(row)(col) = 1
          val neighbours: Seq[(Int, Int)] = findNeighbours(row, col)
          if (neighbours.nonEmpty) {
            val size =
              1 +
                neighbours.collect {
                  case (nRow, nCol) if grid(nRow)(nCol) != 0 =>
                    dfsFrom(nRow, nCol, group)
                }.sum
            currIslandSizes(row)(col) = size
            size
          } else {
            currIslandSizes(row)(col)
          }
        } else {
          0
        }
      }

      val groupMap = fillCurrIslandSizes(0, 0, Map(), 0)
      if (groupMap.isEmpty) 1 else {
        println(groupMap)
        println(currIslandSizes.map(_.mkString(",")).mkString("|"))
        flipZeros(0, 0, 0, groupMap, zeroFound = false)
      }
    }
  }

}
