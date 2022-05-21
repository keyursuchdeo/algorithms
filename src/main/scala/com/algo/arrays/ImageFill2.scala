package com.algo.arrays

object ImageFill2 extends App {
  val a = Array.ofDim[Int](2, 3)
  a(0) = Array(0, 0, 0)
  a(1) = Array(0, 1, 0)
//val a = Array.ofDim[Int](3, 1)
//  a(0) = Array(1)
//  a(1) = Array(1)
//  a(2) = Array(1)

  val res: Array[Array[Int]] = Solution.floodFill(a, 1, 0, 2)
  val b: Array[String] = res.map(_.mkString(","))
  println(b.mkString("|"))

  object Solution {

    def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, newColor: Int): Array[Array[Int]] = {
      val rows = image.length
      val cols = image(0).length

      def cloneImage(): Array[Array[Int]] = {
        val imageToBeFilled = Array.ofDim[Int](rows, cols)
        for (
          i <- 0 until rows;
          j <- 0 until cols
        ) {
          imageToBeFilled(i)(j) = image(i)(j)
        }
        imageToBeFilled
      }

      val filledImage: Array[Array[Int]] = cloneImage()

      def fill(row: Int, col: Int, matchingColor: Int): Unit = {
        if (image(row)(col) == matchingColor) {
          filledImage(row)(col) = newColor
          validNeighboursToColor(row, col, matchingColor).foreach(cell => {
            fill(cell._1, cell._2, matchingColor)
          })
        } else {
          ()
        }
      }

      def validNeighboursToColor(row: Int, col: Int, matchingColor: Int): Seq[(Int, Int)] = {
        val a = if(row - 1 >= 0 && image(row - 1)(col) == matchingColor && filledImage(row - 1)(col) != newColor) {
          Some(row - 1, col)
        } else None
        val b = if (row + 1 < rows && image(row + 1)(col) == matchingColor && filledImage(row + 1)(col) != newColor) {
          Some(row + 1, col)
        } else None
        val c = if (col + 1 < cols && image(row)(col + 1) == matchingColor && filledImage(row)(col + 1) != newColor) {
          Some(row, col + 1)
        } else None
        val d = if (col - 1 >= 0 && image(row)(col - 1) == matchingColor && filledImage(row)(col - 1) != newColor) {
          Some(row, col - 1)
        } else None
        Seq(a, b, c, d).flatten
      }

      val matchingColor = image(sr)(sc)
      fill(sr, sc, matchingColor)
      filledImage
    }
  }

}
