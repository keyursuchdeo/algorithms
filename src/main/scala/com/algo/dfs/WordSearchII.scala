package com.algo.dfs

object WordSearchII extends App {

  object Solution {
    def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
      val rows = board.length
      val cols = board.headOption.map(_.length).getOrElse(0)

      case class Cell(row: Int, col: Int) {
        private val isRowValid = row >= 0 && row <= rows - 1
        private val isColValid = col >= 0 && col <= cols - 1
        require(isRowValid && isColValid)
      }

      def dfs(currBoard: Array[Array[Char]], row: Int, col: Int, currWord: String): Boolean = {
        if (currWord.isEmpty) {
          true
        } else if (row < 0 || row >= rows || col < 0 || col >= cols || currBoard(row)(col) != currWord.head) {
          false
        } else {
          val temp = currBoard(row)(col)
          currBoard(row)(col) = ' '
          val found = dfs(currBoard, row + 1, col, currWord.tail) ||
            dfs(currBoard, row - 1, col, currWord.tail) ||
            dfs(currBoard, row, col + 1, currWord.tail) ||
            dfs(currBoard, row, col - 1, currWord.tail)

          currBoard(row)(col) = temp
          found
        }
      }

      words.filter(word => {
        (0 until rows).exists(row => {
          (0 until cols).exists(col => {
            board(row)(col) == word.head && dfs(board, row, col, word)
          })
        })
      }).toList
    }
  }

}
